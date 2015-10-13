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

data WeakNode = forall a. WeakNode { unNode :: Weak (Node a) }
  
data SomeNode = forall a. SomeNode (Node a)
type NodeId = Int 


newtype NodeRef a = NodeRef { unRef :: IORef (Either (NodeRef a -> EventM (Node a)) (Node a)) }
newtype Event a = Event (NodeRef a)

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
unsafeCreateNode :: (NodeRef a -> EventM (Node a)) -> NodeRef a
unsafeCreateNode action = NodeRef $ unsafePerformIO $ newIORef (Left action) 


createNode :: (NodeRef a -> EventM (Node a)) -> IO (NodeRef a)
createNode action = NodeRef <$> newIORef (Left action) 


unsafeCreateEvent :: (NodeRef a -> EventM (Node a)) -> Event a
unsafeCreateEvent = Event . unsafeCreateNode


createEvent  :: (NodeRef a -> EventM (Node a)) -> IO (Event a)
createEvent action = Event <$> createNode action

readNodeRef :: NodeRef a -> EventM (Node a)
readNodeRef e@(NodeRef ref) = readRef ref >>= \case
    Left action -> do
      node <- action e
      writeRef ref (Right node)
      return node
    Right node -> return node
    
    


eventNode :: Event a -> EventM (Node a)
eventNode (Event ref) = readNodeRef ref

data Function a where
  Push     ::  (a -> EventM (Maybe b)) -> Node a -> Function b
  Merge    ::  Semigroup a => IORef [Node a] -> IntMap (Node a) -> Function a
  Root     ::  Function a


data Node a = Node 
  { nodeHandle    :: !(Weak (NodeRef a))
  , nodeSubs      :: !(IORef [WeakNode])
  , nodeValue     :: !(IORef (Maybe a))
  , nodeFunction  :: !(Function a) 
  , nodeHeight    :: !(IORef Int)  
  , nodeId        :: !NodeId
  }
  
  
readHeight :: Node a -> EventM Int
readHeight node = readRef (nodeHeight node)

weakPtr :: a -> EventM (Weak a)
weakPtr a = liftIO (mkWeakPtr a Nothing)
  
  
newNode :: Int -> NodeRef a -> Function a -> EventM (Node a)
newNode height self f = do
  
  newId <- readRef nextIdRef
  writeRef nextIdRef (succ newId)
  
  Node <$> weakPtr self
       <*> newRef []
       <*> newRef Nothing
       <*> pure f
       <*> newRef height
       <*> pure newId
       
      
      
subscribe :: Node a -> Node b -> EventM ()
subscribe node parent = do
  weakNode <- WeakNode <$> weakPtr node
  modifyRef (nodeSubs parent) (weakNode :)
  
    
push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f e =  unsafeCreateEvent $ \self -> do  
  parent <- eventNode e
  height <- readHeight parent
  node <- newNode height self (Push f parent) 
  subscribe node parent  
  return node

merge :: Semigroup a => [Event a] -> Event a
merge es =  unsafeCreateEvent $ \self -> do  
  
  parents <- traverse eventNode es
  height <- maximum <$> traverse readHeight parents
  let parentMap = IntMap.fromList $ (\p -> (nodeId p, p)) <$> parents
  
  func <- Merge <$> newRef [] <*> pure parentMap
  node <- newNode (succ height) self func 
  traverse_ (subscribe node) parents
  return node

  

eventRoot :: IO (Event a)
eventRoot = createEvent $ \self -> newNode 0 self Root




readNodeMaybe :: Node a -> EventM (Maybe a)
readNodeMaybe node = readRef (nodeValue node)


readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = readNodeMaybe =<< eventNode e
  
  
  


data Trigger where
  Trigger :: a -> Event a -> Trigger


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


trigger :: Trigger -> EventM ()
trigger (Trigger a e) = do
  root <- eventNode e
  writeNode root a
  propagate 0 root
 
 
  
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

propagateDelay :: Height -> SomeNode ->  EventM ()
propagateDelay height (SomeNode node) = do  
  evaluate node >> propagate height node
  


takeDelay :: EventM (Maybe (Height, [SomeNode]))
takeDelay = do
  delaysRef <- asks delays
  delayed <- readRef delaysRef
  
  let view = IntMap.minViewWithKey delayed
  traverse_ (writeRef delaysRef) (snd <$> view) 
  return (fst <$> view)

  
runFrame :: [Trigger] -> EventHandle a -> IO (Maybe a)
runFrame triggers e = runEventM $ do

  traverse_ trigger triggers
  runDelays
  readEvent e <* endFrame
  
  where
    runDelays = do
      mayDelay <- takeDelay
      for_ mayDelay $ \(height, nodes) -> do
        traverse_ (propagateDelay height) nodes  
        runDelays
        
 
  
  
instance Functor Event where
  fmap f e = push (return .   Just . f) e
  
  
  
main = do
  
  input1 <- eventRoot
  input2 <- eventRoot 

  
  let len   =  foldr1 (+) <$> merge [ (pure <$> input1) :: Event [Int], (pure <$> (+1) <$> input2)]
      test1  = merge [ pure <$> input2, pure <$> len ]
      test  = merge [ test1, pure <$> input2 ]
  
  handle <- subscribeEvent test
  x <- runFrame [Trigger 4 input2, Trigger 4 input1] handle
  print (x :: Maybe [Int])

