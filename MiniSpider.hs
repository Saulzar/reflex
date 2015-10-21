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

import Data.Semigroup
import qualified Data.List.NonEmpty as NE



import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap



data SomeNode = forall a. SomeNode { unSome :: Node a }

newtype NodeRef a = NodeRef { unRef :: IORef (Either (EventM (Node a)) (Node a)) }
data Event a = Never | Event (NodeRef a)

newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int


-- data Subscriber a where
--   Push     :: Node b -> (a -> EventM (Maybe b)) -> Subscriber a
--   Merge    ::  GCompare k => Node (DMap k) ->  k a -> Subscriber a
  

type Subscriber a = Height -> a -> EventM ()

-- data Parents where
--   Parent    :: Node a -> Parent
--   MapParent :: DMap (WrapArg Node k) -> Parent
  
  

-- type Subscriber a = Weak (a -> EventM ())


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

  
catEvents :: [Event a] -> [NodeRef a]
catEvents events = [ref | Event ref <- events]  


push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f Never = Never
push f (Event ref) =  unsafeCreateEvent $ do 
  parent <- readNodeRef ref  
  value  <- traverse f =<< readNode parent
  node <- newNode <$> readHeight parent <*> pure [SomeNode parent] <*> pure value
  subscribe parent (\height -> f >=> traverse_ (writePropagate height node))  
  return node  

  
-- merge :: Semigroup a => [Event a] -> Event a
-- merge = merge' . catEvents where
--   merge' []       = Never
--   merge' refs = unsafeCreateEvent $  do  
--     parents <- traverse readNodeRef refs
--     height <- maximum <$> traverse readHeight parents  
--     func <- Merge <$> pure (nodeMap parents) <*> newRef []
--     node <- newNode (succ height) func 
--     traverse_ (subscribe node) parents
--     return (Node node)
  
never :: Event a
never = Never


newEventWithFire :: IO (Event a, a -> Trigger)
newEventWithFire = do
  root <- createNode $ Node <$> newNode 0 [] Nothing
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


propagateDelayed :: Height -> Node a -> EventM ()
propagateDelayed height node = do
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
  return (EventHandle <$> e)
  
  

takeDelay :: EventM (Maybe (Height, [SomeNode]))
takeDelay = do
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
    runDelays = takeDelay >>= traverse_  (\(height, nodes) -> do
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
{-
  
  let len   =  foldr1 (+) <$> merge [ (pure <$> input1) :: Event [Int], (pure <$> (+1) <$> input2), never]
      test1  = merge [ pure <$> input2, pure <$> len ]
      test  = merge [ test1, pure <$> input2 ]-}
  
  handle <- subscribeEvent ((+3) <$> input1) 
  x <- runFrame [fire2 4, fire1 4] handle
  print (x :: Maybe [Int])

