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

import Data.Semigroup
import qualified Data.List.NonEmpty as NE



import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type NodeId = Int 

data WeakNode = forall a. WeakNode { unNode :: Weak (Node a) }
data SomeNode = forall a. SomeNode (Node a)

newtype NodeRef a = NodeRef { unRef :: IORef (Either (EventM (Node a)) (Node a)) }
data Event a = Never | Event (NodeRef a)

newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int

{-# NOINLINE nextIdRef #-}
nextIdRef :: IORef NodeId
nextIdRef = unsafePerformIO $ newIORef 0


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


data Function a where
  Push     ::  (a -> EventM (Maybe b)) -> Node a -> Function b
  Merge    ::  Semigroup a => IORef [Node a] -> IntMap (Node a) -> Function a
  Root     ::  Function a


data Node a = Node 
  { nodeSubs      :: !(IORef [WeakNode])
  , nodeValue     :: !(IORef (Maybe a))
  , nodeFunction  :: !(Function a) 
  , nodeHeight    :: !(IORef Int)  
  , nodeId        :: !NodeId
  }
  
  
readHeight :: Node a -> EventM Int
readHeight node = readRef (nodeHeight node)

weakPtr :: a -> EventM (Weak a)
weakPtr a = liftIO (mkWeakPtr a Nothing)
  
  
newNode :: Height -> Function a -> EventM (Node a)
newNode height f = do
  
  newId <- readRef nextIdRef
  writeRef nextIdRef (succ newId)
  
  Node <$> newRef []
       <*> newRef Nothing
       <*> pure f
       <*> newRef height
       <*> pure newId
       
      
readNodeMaybe :: Node a -> EventM (Maybe a)
readNodeMaybe node = readRef (nodeValue node)


readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = fmap join <$> traverse readNodeMaybe =<< eventNode e
      
      
subscribe :: Node a -> Node b -> EventM ()
subscribe node parent = do
  weakNode <- WeakNode <$> weakPtr node
  modifyRef (nodeSubs parent) (weakNode :)

  
catEvents :: [Event a] -> [NodeRef a]
catEvents events = [ref | Event ref <- events]  

nodeMap :: [Node a] -> IntMap (Node a)
nodeMap = IntMap.fromList . map (\node -> (nodeId node, node))
  

  
-- Event types
push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f Never = Never
push f (Event ref) =  unsafeCreateEvent $ do  
  parent <- readNodeRef ref
  height <- readHeight parent
  node <- newNode height  (Push f parent) 
  subscribe node parent  
  return node
  
  
merge :: Semigroup a => [Event a] -> Event a
merge = merge' . catEvents where
  merge' []       = Never
  merge' refs = unsafeCreateEvent $  do  
    parents <- traverse readNodeRef refs
    height <- maximum <$> traverse readHeight parents  
    func <- Merge <$> newRef [] <*> pure (nodeMap parents)
    node <- newNode (succ height) func 
    traverse_ (subscribe node) parents
    return node
  
never :: Event a
never = Never

  
newEventWithFire :: IO (Event a, a -> Trigger)
newEventWithFire = do
  root <- createNode $ newNode 0 Root
  return (Event root, Trigger root)
  
  


data Trigger where
  Trigger ::  NodeRef a -> a -> Trigger


clearNode :: SomeNode -> IO ()
clearNode (SomeNode node) = do
  writeRef (nodeValue node) Nothing
  case nodeFunction node of
    Merge pushedRef _ -> writeRef pushedRef []
    _                 -> return ()
    
  
writeNode :: Node a -> a -> EventM ()
writeNode node a = do
  clearRef <- asks clears
  writeRef (nodeValue node) (Just a)
  modifyRef clearRef (SomeNode node :)


trigger :: Trigger -> EventM SomeNode
trigger (Trigger ref a) = do
  root <- readNodeRef ref
  writeNode root a
  return (SomeNode root)
 
 
  
readNode :: Node a -> EventM a
readNode node = do
  m <- readNodeMaybe node
  case m of
    Just a  -> return a
    Nothing -> error "readNode failed: node not yet evaluated"



traverseWeak :: (forall a. Node a -> EventM ()) -> [WeakNode] -> EventM [WeakNode]
traverseWeak f subs = do
  flip filterM subs $ \(WeakNode weak) -> do 
    m <- liftIO (deRefWeak weak)
    isJust m <$ mapM_ f m 


modifyM :: MonadRef m => Ref m a -> (a -> m a) -> m ()
modifyM ref f = readRef ref >>= f >>= writeRef ref
    
    
schedule :: Node a -> Height ->  EventM ()    
schedule node height = do
  delayRef <- asks delays
  modifyRef delayRef insert
    where insert = IntMap.insertWith (<>) height [SomeNode node]

propagate ::  Height -> Node a -> EventM () 
propagate  height parent = modifyM  (nodeSubs parent) (traverseWeak propagate') where
  
  propagate' :: forall a. Node a -> EventM ()
  propagate' child = case nodeFunction child of
    Merge ref ps -> forM_ (IntMap.lookup (nodeId parent) ps) $ \node -> 
        modifyM ref $ \pushed -> do
            when (null pushed) $ do
              schedule child =<< readRef (nodeHeight child)
            return (node : pushed)
            
    _ -> evaluate child >> propagate height child

  
    
evaluate :: Node a -> EventM ()
evaluate node = case nodeFunction node of
    Push  f  parent    -> mapM_ (writeNode node) =<< f =<< readNode parent
    Merge ref _  -> do
      parents <- traverse readNode =<< readRef ref
      writeNode node $ sconcat (NE.fromList parents) 
    Root               -> return ()

  

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
  


takeDelay :: EventM (Maybe (Height, [SomeNode]))
takeDelay = do
  delaysRef <- asks delays
  delayed <- readRef delaysRef
  
  let view = IntMap.minViewWithKey delayed
  traverse_ (writeRef delaysRef) (snd <$> view) 
  return (fst <$> view)

  
runFrame :: [Trigger] -> EventHandle a -> IO (Maybe a)
runFrame triggers e = runEventM $ do

  roots <- traverse trigger triggers
  runDelays (0, roots)
  readEvent e <* endFrame
  
  where
    runDelays (height, nodes) = do
      traverse_ (continue height) nodes  
      traverse_ runDelays =<< takeDelay
        
    continue height (SomeNode node) = evaluate node >> propagate height node
  
  
instance Functor Event where
  fmap f e = push (return .   Just . f) e
  
  
  
main = do
  
  (input1, fire1) <- newEventWithFire
  (input2, fire2) <- newEventWithFire 

  
  let len   =  foldr1 (+) <$> merge [ (pure <$> input1) :: Event [Int], (pure <$> (+1) <$> input2), never]
      test1  = merge [ pure <$> input2, pure <$> len ]
      test  = merge [ test1, pure <$> input2 ]
  
  handle <- subscribeEvent test
  x <- runFrame [fire2 4, fire1 4] handle
  print (x :: Maybe [Int])

