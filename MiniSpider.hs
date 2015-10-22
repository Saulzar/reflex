{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}




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
import Data.Void

import Data.Functor.Misc

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap, GCompare)

import Data.Dependent.Sum 


import Data.Semigroup
import qualified Data.List.NonEmpty as NE



import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap



data SomeNode = forall a. SomeNode { unSome :: Node a }

-- data MakeNode a where
--  MakePush :: Node a -> (a -> EventM (Maybe b)) -> MakeNode b
--  MakeMerge :: GCompare k => DMap (WrapArg Event k) -> MakeNode (DMap k)


type MakeNode a = EventM (Node a)

newtype NodeRef a = NodeRef { unRef :: IORef (Either (MakeNode a) (Node a)) }
data Event a = Never | Event (NodeRef a)

newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int


-- Data structure based Subscriber

-- data Subscriber a where
--   Push     :: Node b -> (a -> EventM (Maybe b)) -> Subscriber a
--   Merge    ::  GCompare k => Node (DMap k) ->  k a -> Subscriber a


-- data Parents where
--   Parent    :: Node a -> Parent
--   MapParent :: DMap (WrapArg Node k) -> Parent

  

type Subscriber a = Height -> a -> EventM ()



data Node a = Node 
  { nodeSubs      :: !(IORef [Weak (Subscriber a)])
  , nodeHeight    :: !(IORef Int)  
  , nodeParents   :: ![SomeNode]
  , nodeValue     :: !(IORef (Maybe a))
  }


data Env = Env 
  { clears :: IORef [SomeNode]
  , delays :: IORef (IntMap [SomeNode])
  } 
  
newtype EventM a = EventM { unEventM :: ReaderT Env IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)


instance MonadRef EventM where
  type Ref EventM = Ref IO
  {-# INLINE newRef #-}
  {-# INLINE readRef #-}
  {-# INLINE writeRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a
  
  

{-# NOINLINE unsafeCreateNode #-}
unsafeCreateNode :: EventM (Node a) -> NodeRef a
unsafeCreateNode create = NodeRef $ unsafePerformIO $ newIORef (Left create) 


createNode :: EventM (Node a) -> IO (NodeRef a)
createNode create = NodeRef <$> newIORef (Left create) 


unsafeCreateEvent :: EventM (Node a) -> Event a
unsafeCreateEvent = Event . unsafeCreateNode


createEvent  :: EventM (Node a) -> IO (Event a)
createEvent create = Event <$> createNode create

readNodeRef :: NodeRef a -> EventM (Node a)
readNodeRef (NodeRef ref) = readRef ref >>= \case
    Left create -> do
      node <- create
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
  
  
newNode :: Height -> [SomeNode] -> Maybe a -> EventM (Node a)
newNode height parents value = 
  Node <$> newRef [] <*> newRef height <*> pure parents <*> newRef value
       

readNode :: Node a -> EventM (Maybe a)
readNode node = readRef (nodeValue node)

writeNode :: Node a -> a -> EventM ()
writeNode node a = do
  writeRef (nodeValue node) (Just a)
  clearsRef <- asks clears
  modifyRef clearsRef (SomeNode node :)
  
readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = fmap join <$> traverse readNode =<< eventNode e

  
subscribe :: Node a -> Subscriber a -> EventM ()
subscribe node sub = do
  weakSub <- weakPtr sub
  modifyRef (nodeSubs node) (weakSub :)



push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f Never = Never
push f (Event ref) =  unsafeCreateEvent $ do 
  parent <- readNodeRef ref  
  value  <- traverse f =<< readNode parent
  node <- join $ newNode <$> readHeight parent <*> pure [SomeNode parent] <*> pure (join value)
  subscribe parent $ \height -> 
    f >=> traverse_ (writePropagate height node)
  return node  
  
  

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
  

merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge = merge' . catEvents . DMap.toAscList where
  merge' []       = Never
  merge' refs = unsafeCreateEvent $  do     
    parents <- traverseDSums readNodeRef refs
    height <- maximum <$> sequence (mapDSums readHeight parents)
    values <- catDSums <$> traverseDSums readNode parents
    node <- newNode height (mapDSums SomeNode parents) (DMap.fromDistinctAscList values <$ listToMaybe values)    
    traverse_ (subscribe' node) parents
    return node

  subscribe' :: GCompare k => Node (DMap k) -> DSum (WrapArg Node k) -> EventM ()
  subscribe' node (WrapArg k :=> parent) = subscribe parent $ \height value -> do
    modifyM (nodeValue node) $ \case 
      Nothing -> do
        delay node =<< readHeight node
        return (Just $ DMap.singleton k value)
      Just m  -> pure . Just $ DMap.insert k value m

  
never :: Event a
never = Never


newEventWithFire :: IO (Event a, a -> Trigger)
newEventWithFire = do
  root <- createNode $ newNode 0 [] Nothing
  return (Event root, Trigger root)
  


data Trigger where
  Trigger ::  NodeRef a -> a -> Trigger


    
clearNode :: SomeNode -> IO ()
clearNode (SomeNode node) = writeRef (nodeValue node) Nothing
    

traverseWeak :: (a -> EventM ()) -> [Weak a] -> EventM [Weak a]
traverseWeak f subs = do
  flip filterM subs $ \weak -> do 
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
   
    
propagate ::  Height -> Node a -> a -> EventM () 
propagate  height node value = modifyM  (nodeSubs node) $ 
  traverseWeak (\prop -> prop height value)


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
  
  
main = do
  
  (input1, fire1) <- newEventWithFire
  (input2, fire2) <- newEventWithFire 
  
  
  let out = mergeList [input1, input2, (+1) <$> input1, (+2) <$> input2]
  
  
  handle <- subscribeEvent out
  x <- runFrame [fire2 4, fire1 2] handle
  print (x :: Maybe [Int])


  
  
-- From Reflex
eventDMap :: [Event a] -> DMap (WrapArg Event (Const2 Int a))
eventDMap es = DMap.fromDistinctAscList $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es

fromDMap :: DMap (Const2 Int a) -> [a]
fromDMap = map (\(Const2 _ :=> v) -> v) . DMap.toList
       
mergeWith :: (a -> a -> a) -> [Event a] -> Event a
mergeWith f es =  foldl1 f <$> mergeList es
  
mergeList ::  [Event a] -> Event [a]
mergeList es = fromDMap <$> merge (eventDMap es) 