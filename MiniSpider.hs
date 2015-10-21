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

import Data.Semigroup
import qualified Data.List.NonEmpty as NE



import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type NodeId = Int 


data SomeNode = forall a. SomeNode { unSome :: Node a }

newtype NodeRef a = NodeRef { unRef :: IORef (Either (EventM (OutNode a)) (OutNode a)) }
data Event a = Never | Event (NodeRef a)

newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int


data Subscriber a where
  Push     :: Node b -> (a -> EventM (Maybe b)) -> Subscriber a
  Merge    ::  GCompare k => Node (DMap k) ->  k a -> Subscriber a
  


data Parent a where
  Parent    :: Node b -> Parent a
  MapParent :: DMap (WrapArg Node k) -> Parent a
  
  

-- type Subscriber a = Weak (a -> EventM ())


data Node a = Node 
  { nodeSubs      :: !(IORef [Weak (Subscriber a)])
  , nodeValue     :: !(IORef a)
  , nodeHeight    :: !(IORef Int)  
  , nodeId        :: !NodeId
  , nodeParents     :: !(Parent a)
  }

  


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
unsafeCreateNode :: EventM (OutNode a) -> NodeRef a
unsafeCreateNode create = NodeRef $ unsafePerformIO $ newIORef (Left create) 


createNode :: EventM (OutNode a) -> IO (NodeRef a)
createNode create = NodeRef <$> newIORef (Left create) 


unsafeCreateEvent :: EventM (OutNode a) -> Event a
unsafeCreateEvent = Event . unsafeCreateNode


createEvent  :: EventM (OutNode a) -> IO (Event a)
createEvent create = Event <$> createNode create

readNodeRef :: NodeRef a -> EventM (OutNode a)
readNodeRef (NodeRef ref) = readRef ref >>= \case
    Left create -> do
      node <- create
      writeRef ref (Right node)
      return node
    Right node -> return node
    
    
eventNode :: Event a -> EventM (Maybe (OutNode a))
eventNode Never       = return Nothing
eventNode (Event ref) = Just <$> readNodeRef ref


outId :: OutNode a -> NodeId
outId (OutNode node) = nodeId node
  
readHeight :: OutNode a -> EventM Int
readHeight (OutNode node) = readRef (nodeHeight node)

weakPtr :: a -> EventM (Weak a)
weakPtr a = liftIO (mkWeakPtr a Nothing)
  
  
newNode :: Height -> Function b a -> EventM (Node b a)
newNode height f = do
  
  newId <- readRef nextIdRef
  writeRef nextIdRef (succ newId)
  
  Node <$> newRef []
       <*> pure f
       <*> newRef height
       <*> pure newId
       

      
readNodeMaybe :: OutNode a -> EventM (Maybe a)
readNodeMaybe (OutNode node) = case nodeFunction node of 
  Memo _ ref -> readRef ref
  _          -> return Nothing

readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = fmap join <$> traverse readNodeMaybe =<< eventNode e

  
subscribe :: Node b c  -> OutNode b -> EventM ()
subscribe node (OutNode parent) = do
  weakSub <- WeakSub <$> weakPtr node
  modifyRef (nodeSubs parent) (weakSub :)

  
catEvents :: [Event a] -> [NodeRef a]
catEvents events = [ref | Event ref <- events]  

nodeMap :: [OutNode a] -> IntMap (OutNode a)
nodeMap = IntMap.fromList . map (\node -> (outId node, node))
  

simpleEvent :: (OutNode a -> EventM (Function a b)) -> Event a -> Event b
simpleEvent makeFunc Never = Never
simpleEvent makeFunc (Event ref) =  unsafeCreateEvent $ do  
  parent <- readNodeRef ref
  height <- readHeight parent
  node <- newNode height  =<< makeFunc parent
  subscribe node parent  
  return (OutNode node)
  
-- Event types
push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f = simpleEvent (\parent -> return $ Push parent f)

  
memo :: Event a -> Event a  
memo = simpleEvent $ \parent -> Memo parent <$> newRef Nothing

  
merge :: Semigroup a => [Event a] -> Event a
merge = merge' . catEvents where
  merge' []       = Never
  merge' refs = unsafeCreateEvent $  do  
    parents <- traverse readNodeRef refs
    height <- maximum <$> traverse readHeight parents  
    func <- Merge <$> pure (nodeMap parents) <*> newRef []
    node <- newNode (succ height) func 
    traverse_ (subscribe node) parents
    return (OutNode node)
  
never :: Event a
never = Never


newEventWithFire :: IO (Event a, a -> Trigger)
newEventWithFire = do
  root <- createNode $ OutNode <$> newNode 0 Root
  return (Event root, Trigger root)
  


data Trigger where
  Trigger ::  NodeRef a -> a -> Trigger


    
clearNode :: SomeNode -> IO ()
clearNode (SomeNode node) = do
  case nodeFunction node of
    _                 -> return ()
    


readNode :: OutNode a -> EventM a
readNode node = do
  m <- readNodeMaybe node
  case m of
    Just a  -> return a
    Nothing -> error "readNode failed: node not yet evaluated"



traverseWeak :: (forall a. Node b a -> EventM ()) -> [WeakSub b] -> EventM [WeakSub b]
traverseWeak f subs = do
  flip filterM subs $ \(WeakSub weak) -> do 
    m <- liftIO (deRefWeak weak)
    isJust m <$ mapM_ f m 


modifyM :: MonadRef m => Ref m a -> (a -> m a) -> m ()
modifyM ref f = readRef ref >>= f >>= writeRef ref
    
    
delay :: Node b a -> Height ->  EventM ()    
delay node height = do
  delayRef <- asks delays
  modifyRef delayRef insert
    where insert = IntMap.insertWith (<>) height [SomeNode node]

propagateFrom ::  Height -> a -> Node b a -> EventM () 
propagateFrom  height a parent = modifyM  (nodeSubs parent) $ 
  traverseWeak (propagateTo height a)


propagateTo :: Height -> b -> Node b a -> EventM ()
propagateTo height b child = case nodeFunction child of
  Merge _ ref -> modifyM ref $ \bs -> do
    when (null bs) $ do
      delay child =<< readRef (nodeHeight child)
    return (b : bs)
      
  Push _ f    -> f b >>= traverse_ (\a -> propagateFrom height a child)
  Memo _ ref  -> writeRef ref (Just b) >> propagateFrom height b child
  Root        -> error "propagate': root nodes should not recieve events"
          
          
    
   
propagateDelayed :: Height -> SomeNode -> EventM ()
propagateDelayed height (SomeNode node) = case nodeFunction node of
    Merge _ ref  -> do
      a <- sconcat . NE.fromList <$> readRef ref
      writeRef ref []
      propagateFrom height a node
      
    _ -> error "propagateDelayed: unexpected delayed node type"



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
  void (eventNode m)
  return $ EventHandle m
    where m = memo e
  

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
      (OutNode node) <- readNodeRef nodeRef 
      propagateFrom 0 a node

  
  
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

