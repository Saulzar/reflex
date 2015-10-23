{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


import Criterion.Main
import Data.List.Split



import Data.IORef
import System.Mem.Weak

import System.IO.Unsafe
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.IO.Class

import Data.Foldable
import Data.Traversable
import Data.Functor
import Data.Maybe
import Data.List
import Data.Void

import Data.Functor.Misc

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap, GCompare)

import Data.Dependent.Sum 

import Data.Semigroup
import qualified Data.List.NonEmpty as NE

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data SomeNode = forall a. SomeNode { unSome :: Node a }

data MakeNode a where
  MakePush  :: NodeRef a -> (a -> EventM (Maybe b)) -> MakeNode b
  MakeMerge :: GCompare k => [DSum (WrapArg NodeRef k)] -> MakeNode (DMap k)
  MakeRoot  :: MakeNode a


newtype NodeRef a = NodeRef { unRef :: IORef (Either (MakeNode a) (Node a)) }
data Event a = Never | Event (NodeRef a)

newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int

data Subscription a b where
  Push     :: Node a -> Node b -> (a -> EventM (Maybe b)) -> Subscription a b
  Merge    :: GCompare k => Node a -> Node (DMap k) -> k a -> Subscription a (DMap k)
  MergeMap :: Ord k => Node a -> Node (Map k a) -> k -> Subscription a (Map k a)

data Subscribing b = forall a. Subscribing (Subscription a b)
data WeakSubscriber a = forall b. WeakSubscriber { unWeak :: Weak (Subscription a b) }


data Node a = Node 
  { nodeSubs      :: !(IORef [WeakSubscriber a])
  , nodeHeight    :: !(IORef Int)  
  , nodeParents   :: ![Subscribing a]
  , nodeValue     :: !(IORef (Maybe a))
  }


data Env = Env 
  { clears :: !(IORef [SomeNode])
  , delays :: !(IORef (IntMap [SomeNode]))
  } 
  
newtype EventM a = EventM { unEventM :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadReader Env)


instance MonadRef EventM where
  type Ref EventM = Ref IO
  {-# INLINE newRef #-}
  {-# INLINE readRef #-}
  {-# INLINE writeRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a
  
  

unsafeCreateNode :: MakeNode a -> NodeRef a
unsafeCreateNode create = NodeRef $ unsafePerformIO $ newIORef (Left create) 


makeNode :: MakeNode a -> IO (NodeRef a)
makeNode create = NodeRef <$> newIORef (Left create) 


unsafeCreateEvent :: MakeNode a -> Event a
unsafeCreateEvent = Event . unsafeCreateNode


createEvent  :: MakeNode a -> IO (Event a)
createEvent create = Event <$> makeNode create

readNodeRef :: NodeRef a -> EventM (Node a)
readNodeRef (NodeRef ref) = readRef ref >>= \case
    Left create -> do
      node <- createNode create
      writeRef ref (Right node)
      return node
    Right node -> return node
    
    
eventNode :: Event a -> EventM (Maybe (Node a))
eventNode Never       = return Nothing
eventNode (Event ref) = Just <$> readNodeRef ref


  
readHeight :: Node a -> EventM Int
readHeight node = readRef (nodeHeight node)

weakPtr :: a -> EventM (Weak a)
weakPtr a = liftIO (mkWeakPtr a Nothing)

newNode :: Height -> [Subscribing a] -> EventM (Node a)
newNode height parents = 
  Node <$> newRef [] <*> newRef height <*> pure parents <*> newRef Nothing
       

readNode :: Node a -> EventM (Maybe a)
readNode node = readRef (nodeValue node)


writeNode :: Node a -> a -> EventM ()
writeNode node a = do
  writeRef (nodeValue node) (Just a)
  clearsRef <- asks clears
  modifyRef clearsRef (SomeNode node :)
  
readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = fmap join <$> traverse readNode =<< eventNode e

  
subscribe :: Node a -> Subscription a b -> EventM ()
subscribe node sub = do
  weakSub <- weakPtr sub
  modifyRef (nodeSubs node) (WeakSubscriber weakSub :)
  

createNode :: MakeNode a -> EventM (Node a)
createNode (MakePush ref f) = makePush ref f
createNode (MakeMerge refs) = makeMerge refs 
createNode MakeRoot = newNode 0 []

  
push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f Never = Never
push f (Event ref) = unsafeCreateEvent $ MakePush ref f


makePush :: NodeRef a -> (a -> EventM (Maybe b)) -> EventM (Node b)
makePush ref f =  do 
  parent <- readNodeRef ref
  height <- readHeight parent
  rec
    let sub = Push parent node f
    node <- newNode height [Subscribing sub]
  
  readNode parent >>= traverse (\a -> do
    f a >>= traverse (writeNode node))
  
  subscribe parent sub
  return node  
  

merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge events = case catEvents (DMap.toAscList events) of
  [] -> Never
  refs -> unsafeCreateEvent (MakeMerge refs) 


mergeSubscribing :: GCompare k => Node (DMap k) -> DSum (WrapArg Node k) -> EventM (Subscribing (DMap k))  
mergeSubscribing node (WrapArg k :=> parent) = do
  subscribe parent sub
  return (Subscribing sub)
  
  where sub = Merge parent node k

  
makeMerge :: GCompare k => [DSum (WrapArg NodeRef k)] -> EventM (Node (DMap k))
makeMerge refs = do     
  parents <- traverseDSums readNodeRef refs
  height <- maximum <$> sequence (mapDSums readHeight parents) 
  values <- catDSums <$> traverseDSums readNode parents
  rec
    subs <- traverse (mergeSubscribing node) parents
    node <- newNode (succ height) subs 
    
  when (not  . null  $ values) $ writeNode node (fromAsc values)
  return node
  
  
never :: Event a
never = Never


newEventWithFire :: IO (Event a, a -> Trigger)
newEventWithFire = do
  root <- makeNode MakeRoot
  return (Event root, Trigger root)
  


data Trigger where
  Trigger ::  NodeRef a -> a -> Trigger


    
clearNode :: SomeNode -> IO ()
clearNode (SomeNode node) = writeRef (nodeValue node) Nothing
    

traverseWeak :: (forall b. Subscription a b -> EventM ()) -> [WeakSubscriber a] -> EventM [WeakSubscriber a]
traverseWeak f subs = do
  flip filterM subs $ \(WeakSubscriber weak) -> do 
    m <- liftIO (deRefWeak weak)
    isJust m <$ traverse_ f m 


modifyM :: MonadRef m => Ref m a -> (a -> m a) -> m ()
modifyM ref f = readRef ref >>= f >>= writeRef ref
    
    
delay :: Node a -> Height ->  EventM ()    
delay node height = do
  delayRef <- asks delays
  modifyRef delayRef insert
    where insert = IntMap.insertWith (<>) height [SomeNode node]

    
writePropagate ::  Height -> Node a -> a -> EventM () 
writePropagate  height node value = do
  writeNode node value
  propagate height node value
  

    
propagate :: forall a. Height -> Node a -> a -> EventM () 
propagate  height node value = modifyM  (nodeSubs node) $ traverseWeak propagate' where
  
  propagate' :: Subscription a b -> EventM ()
  propagate' (Push _ dest f)  = 
    f value >>= traverse_ (writePropagate height dest)

  propagate' (Merge _ dest k) = do
    v <- readNode dest
    case v of
      Nothing -> do
        delay dest =<< readHeight dest
        writeNode dest (DMap.singleton k value)
      Just m  -> writeRef (nodeValue dest) $ Just (DMap.insert k value m)
  


propagateDelayed :: Height -> SomeNode -> EventM ()
propagateDelayed height (SomeNode node) = do
  readNode node >>= traverse_ (propagate height node)
  

runEventM :: EventM a -> IO a
runEventM (EventM action) = do
  env <- Env <$> newRef [] <*> newRef mempty  
  runReaderT action env
  
  
endFrame :: EventM ()
endFrame = do
  cs <- readRef =<< asks clears
  liftIO $ traverse_ clearNode cs 
  

  
subscribeEvent :: Event a -> IO (EventHandle a)  
subscribeEvent e = runEventM $ do
  void (eventNode e)
  return (EventHandle e)
  
  

takeDelayed :: EventM (Maybe (Height, [SomeNode]))
takeDelayed = do
  delaysRef <- asks delays
  delayed <- readRef delaysRef
  
  let view = IntMap.minViewWithKey delayed
  traverse_ (writeRef delaysRef) (snd <$> view) 
  return (fst <$> view)

  
runFrame :: [Trigger] -> EventHandle a -> IO (Maybe a)
runFrame triggers e = runEventM $ do

  roots <- traverse propagateRoot triggers
  runDelays
  readEvent e <* endFrame
  
  where
    runDelays = takeDelayed >>= traverse_  (\(height, nodes) -> do
        traverse_ (propagateDelayed height) nodes  
        runDelays)
        
    propagateRoot (Trigger nodeRef a) = do
      node <- readNodeRef nodeRef 
      writePropagate 0 node a

  
  
instance Functor Event where
  fmap f e = push (return .   Just . f) e
  
  
mergeTree ::  Int -> [Event Int] -> Event Int
mergeTree n es | length es <= n = mergeWith (+) es
               | otherwise = mergeTree n subTrees
  where
    merges = chunksOf n es
    subTrees = map (mergeWith (+)) merges
  

makeTree size branching = do
  (es, fires) <- unzip <$> for [1..size] (const newEventWithFire)
  return (mergeTree branching es, fires)
  
  
mergeSubscribe size branching = do
  (e, fires) <- makeTree size branching
  subscribeEvent e
   
  
mergeFiring size branching = do
  (e, fires) <- makeTree size branching
  handle <- subscribeEvent e  
  
  for_ (transpose $ chunksOf 7 fires) $ \firing -> 
    runFrame (zipWith ($) firing [1..]) handle
    
--     total <- runFrame (zipWith ($) firing [1..]) handle
--     print (total, sum [1..length firing]) 

    
main = defaultMain 
  [ bench "mergeSubscribe" $ whnfIO $ mergeSubscribe size branching
  , bench "mergeFiring" $ whnfIO $ mergeFiring size branching
  ]  
  
  where 
    size      = 10000
    branching = 4
  
-- main = do

--   (input1, fire1) <- newEventWithFire
--   (input2, fire2) <- newEventWithFire 
--   
--   let out = mergeList [input1, input2, (+1) <$> input1, (+2) <$> input2]
--   
--   handle <- subscribeEvent (out)
--   x <- runFrame [fire1 (3::Int)] handle
--   print x
--   
  
--   let size = 200
--   (e, fires) <- makeTree size 2
--   
--   handle <- subscribeEvent e
--   
--   print =<< runFrame (zipWith ($) fires [1..]) handle
--   print =<< runFrame (zipWith ($) fires [1..]) handle
--   print =<< runFrame (zipWith ($) fires [1..]) handle
-- 
--   
-- --   print (x :: Maybe Int)
--   print (sum [1..size])


  
  
-- From Reflex
eventDMap :: [Event a] -> DMap (WrapArg Event (Const2 Int a))
eventDMap es = fromAsc $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es

fromDMap :: DMap (Const2 Int a) -> [a]
fromDMap = map (\(Const2 _ :=> v) -> v) . DMap.toList
       
mergeWith :: (a -> a -> a) -> [Event a] -> Event a
mergeWith f es =  foldl1 f <$> mergeList es
  
mergeList ::  [Event a] -> Event [a]
mergeList es = fromDMap <$> merge (eventDMap es) 


-- DMap utilities
catEvents ::  [DSum (WrapArg Event k)] -> [DSum (WrapArg NodeRef k)]
catEvents events = [(WrapArg k) :=> ref | (WrapArg k) :=> Event ref <- events]   

traverseDSums :: Applicative m => (forall a. f a -> m (g a)) -> [DSum (WrapArg f k)] -> m [DSum (WrapArg g k)]
traverseDSums f = traverse (\(WrapArg k :=> v) -> (WrapArg k :=>) <$> f v)
  
mapDSums :: (forall a. f a -> b) -> [DSum (WrapArg f k)] -> [b]
mapDSums f = map (\(WrapArg k :=> v) -> f v)

catDSums :: [DSum (WrapArg Maybe k)] -> [DSum k]
catDSums = catMaybes . map toMaybe
  
toMaybe :: DSum (WrapArg Maybe k)  -> Maybe (DSum k)
toMaybe (WrapArg k :=> Just v ) = Just (k :=> v)
toMaybe (WrapArg k :=> Nothing) = Nothing

fromAsc :: [DSum k] -> DMap k
fromAsc = DMap.fromDistinctAscList