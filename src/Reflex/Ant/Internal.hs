{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDecls #-}

module Reflex.Ant.Internal where


import qualified Reflex.Class as R
import qualified Reflex.Host.Class as R

import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Primitive

import Data.Semigroup
import Data.Foldable
import Data.Maybe
import Data.Functor.Misc

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum
import Data.GADT.Compare

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.IORef
import System.Mem.Weak
import System.IO.Unsafe
import Unsafe.Coerce

data MakeNode a where
  MakePush        :: NodeRef a -> (a -> EventM (Maybe b)) -> MakeNode b
  MakeSwitch      :: Behavior (Event a) -> MakeNode a
  MakeCoincidence :: !(NodeRef (Event a)) -> MakeNode a
  MakeRoot        :: GCompare k => k a -> Root k -> MakeNode a
  MakeMerge       :: GCompare k => [DSum (WrapArg NodeRef k)] -> MakeNode (DMap k)
  MakeFan         :: GCompare k => k a -> FanRef k -> MakeNode a


type LazyRef a b = IORef (Either a b)

type LazyNode a = LazyRef (MakeNode a) (Node a)
newtype NodeRef a = NodeRef (LazyNode a)

type FanRef  k = LazyRef (NodeRef (DMap k)) (Fan k)
type PullRef a = LazyRef (BehaviorM a) (Pull a)

data Event a
  = Never
  | Event !(NodeRef a)

data Behavior a
  = Constant a
  | PullB (PullRef a)
  | HoldB (Hold a)


newtype Trigger a = Trigger (Node a)
newtype EventHandle a = EventHandle { unEventHandle :: Maybe (Node a) }

newtype EventSelector k = EventSelector { select :: forall a. k a -> Event a }

type Height = Int
type Key = Int

data Subscription a where
  PushSub   :: Push a b -> Subscription a
  MergeSub  :: GCompare k => Merge k -> k a -> Subscription a
  FanSub    :: GCompare k => Fan k -> Subscription (DMap k)
  HoldSub   :: Hold a -> Subscription a
  SwitchSub :: Switch a -> Subscription a
  CoinInner :: Coincidence a -> Subscription a
  CoinOuter :: Coincidence a -> Subscription (Event a)

instance Show (Subscription a) where
  show (PushSub   {}) = "Push"
  show (MergeSub  {}) = "Merge"
  show (HoldSub   {}) = "Hold"
  show (SwitchSub {}) = "Switch"
  show (CoinInner {}) = "Inner Coincidence"
  show (CoinOuter {}) = "Outer Coincidence"
  show (FanSub    {}) = "Fan"


data Subscribing b where
  SubscribingPush   :: Push a b -> Subscribing b
  SubscribingMerge  :: Merge k -> Subscribing (DMap k)
  SubscribingSwitch :: Switch a -> Subscribing a
  SubscribingCoin   :: Coincidence a -> Subscribing a
  SubscribingFan    :: Fan k -> k a -> Subscribing a
  SubscribingRoot   :: Maybe (IO ()) -> Subscribing b

data Node a = Node
  { nodeSubs      :: !(IORef (IntMap (Subscription a)))
  , nodeHeight    :: !(IORef Int)
  , nodeParents   :: (Subscribing a)
  , nodeOcc       :: !(IORef (Maybe a))
  , nodeHandle    :: !(Weak (LazyNode a))
  }


data Invalidator where
  PullInv   :: Pull a   -> Invalidator
  SwitchInv :: Switch a -> Invalidator


data Push a b = Push
  { pushNode    :: Node b
  , pushParent  :: NodeSub a
  , pushCompute :: a -> EventM (Maybe b)
  }

data Pull a  = Pull
  { pullInv     :: Invalidator
  , pullInvs    :: !(IORef [Weak Invalidator])
  , pullValue   :: !(IORef (Maybe a))
  , pullCompute :: (BehaviorM a)
  }

data Hold a = Hold
  { holdInvs     :: !(IORef [Weak Invalidator])
  , holdValue    :: !(IORef a)
  , holdSub      :: !(LazyRef (Event a) (Maybe (NodeSub a)))
  }

data NodeSub a = NodeSub !(Node a) !Key

data Switch a = Switch
  { switchNode   :: Node a
  , switchConn   :: !(IORef (Maybe (NodeSub a)))
  , switchInv    :: Invalidator
  , switchSource :: Behavior (Event a)
  }

data Coincidence a = Coincidence
  { coinNode     :: Node a
  , coinParent   :: NodeSub (Event a)
  }


data Merge k = Merge
  { mergeNode     :: Node (DMap k)
  , mergeParents  :: DMap (WrapArg NodeSub k)
  , mergePartial  :: !(IORef (DMap k))
  }


data Fan k  = Fan
  { fanParent   :: NodeSub (DMap k)
  , fanNodes    :: IORef (DMap (WrapArg Node k))
  }

data Root k  = Root
  { rootNodes :: IORef (DMap (WrapArg Node k))
  , rootInit  :: (forall a. k a -> Trigger a -> IO (IO ()))
  }

-- A bunch of existentials used so we can put these things in lists
data WriteHold      where WriteHold      :: !(Hold a)    -> !a -> WriteHold
data HoldInit       where HoldInit       :: !(Hold a)    -> HoldInit
data Connect        where Connect        :: !(Switch a)  -> Connect
data DelayMerge     where DelayMerge     :: !(Merge k)   -> DelayMerge
data CoincidenceOcc where CoincidenceOcc :: !(NodeSub a) ->  CoincidenceOcc
data SomeNode       where SomeNode       :: !(Node a)    -> SomeNode


-- EvenM environment, lists of things which need attention at the end of a frame
data Env = Env
  { envDelays       :: !(IORef (IntMap [DelayMerge]))
  , envClears       :: !(IORef [SomeNode])
  , envHolds        :: !(IORef [WriteHold])
  , envHoldInits    :: !(IORef [HoldInit])
  , envCoincidences :: !(IORef [CoincidenceOcc])
  }

newtype EventM a = EventM { unEventM :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadReader Env)

newtype BehaviorM a = BehaviorM { unBehaviorM :: ReaderT (Weak Invalidator) IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadReader (Weak Invalidator))


type MonadIORef m = (MonadIO m, MonadRef m, Ref m ~ IORef)

instance MonadRef EventM where
  type Ref EventM = Ref IO
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a

instance MonadRef BehaviorM where
  type Ref BehaviorM = Ref IO
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a


{-# NOINLINE unsafeLazy #-}
unsafeLazy :: a -> LazyRef a b
unsafeLazy create = unsafePerformIO $ newIORef (Left create)


makeNode :: MakeNode a -> IO (NodeRef a)
makeNode create = NodeRef <$> newIORef (Left create)


unsafeCreateEvent :: MakeNode a -> Event a
unsafeCreateEvent = Event . NodeRef . unsafeLazy


createEvent  :: MakeNode a -> IO (Event a)
createEvent create = Event <$> makeNode create


readLazy :: MonadIORef m => (a -> m b) -> LazyRef a b -> m b
readLazy create ref = readRef ref >>= \case
    Left a -> do
      node <- create a
      writeRef ref (Right node)
      return node
    Right node -> return node

readNodeRef :: NodeRef a -> EventM (Node a)
readNodeRef r@(NodeRef ref) = readLazy (createNode r) ref


eventNode :: Event a -> EventM (Maybe (Node a))
eventNode Never       = return Nothing
eventNode (Event ref) = Just <$> readNodeRef ref


readHeight :: MonadIORef m => Node a -> m Int
readHeight node = readRef (nodeHeight node)


newNode :: MonadIO m => NodeRef a -> Height -> Subscribing a -> m (Node a)
newNode (NodeRef ref) height parents = liftIO $ do
  rec
    node <- Node <$> newRef mempty <*> newRef height <*> pure parents <*> newRef Nothing <*> mkWeakIORef ref (finalizer node)
  return node

  where
    finalizer node = do
      subs <- readRef (nodeSubs node)
      when (null subs) $ unsubscribeNode parents


readNode :: MonadIORef m => Node a -> m (Maybe a)
readNode node = readRef (nodeOcc node)


writeNode :: Node a -> a -> EventM ()
writeNode node a = do
  writeRef (nodeOcc node) (Just a)
  clearsRef <- asks envClears
  modifyRef clearsRef (SomeNode node :)

readEvent :: EventHandle a -> IO (Maybe a)
readEvent (EventHandle n) = join <$> traverse readNode n


subscribe :: MonadIORef m => Node a -> Subscription a -> m (NodeSub a)
subscribe node sub = do
  subs <- readRef (nodeSubs node)
  let k = next subs
  writeRef (nodeSubs node) (IntMap.insert k sub subs)
  return (NodeSub node k)

  where
    next m | null m    = 0
           | otherwise = succ (fst (IntMap.findMax m))


unsubscribeNode :: Subscribing a -> IO ()
unsubscribeNode (SubscribingPush p) = unsubscribe (pushParent p)
unsubscribeNode (SubscribingSwitch s) = do
  conn <- readRef (switchConn s)
  traverse_ unsubscribe conn

unsubscribeNode (SubscribingCoin c) = unsubscribe (coinParent c)
unsubscribeNode (SubscribingRoot finalizer) = sequence_ finalizer

unsubscribeNode (SubscribingFan _ _) = error "not implemented yet"

unsubscribeNode (SubscribingMerge m) = traverse_ unsub (DMap.toList $ mergeParents m)
  where
    unsub :: forall k. DSum (WrapArg NodeSub k) -> IO ()
    unsub (WrapArg _ :=> sub) = unsubscribe sub


unsubscribe :: NodeSub a -> IO ()
unsubscribe (NodeSub node key) = do
  subs <- readRef (nodeSubs node)
  let subs' = IntMap.delete key subs
  writeRef (nodeSubs node) subs'

  when (null subs') $ do
    m <- deRefWeak (nodeHandle node)
    when (isNothing m) $ unsubscribeNode (nodeParents node)

createNode :: NodeRef a -> MakeNode a -> EventM (Node a)
createNode self  (MakePush ref f)      = makePush self ref f
createNode self (MakeMerge refs)      = makeMerge self refs
createNode self (MakeSwitch b)        = makeSwitch self b
createNode self (MakeCoincidence ref) = makeCoincidence self ref
createNode self (MakeFan k f)         = makeFanNode self f k
createNode self (MakeRoot root k)     = makeRoot self root k

constant :: a -> Behavior a
constant = Constant

hold :: a -> Event a -> EventM (Behavior a)
hold a e = do
  h <- Hold <$> newRef [] <*> newRef a <*> newRef (Left e)
  delayInitHold h
  return $ HoldB h

initHold :: HoldInit -> EventM ()
initHold (HoldInit h) = void $ readLazy createHold (holdSub h) where
  createHold (Never) = return  Nothing
  createHold (Event ref) = do
    parent <- readNodeRef ref
    value <- readNode parent
    traverse_ (delayHold h) value

    Just <$> subscribe parent (HoldSub h)



pull :: BehaviorM a -> Behavior a
pull = PullB . unsafeLazy

createPull :: BehaviorM a -> IO (Pull a)
createPull f = do
  rec
    p <- Pull <$> pure (PullInv p) <*> newRef [] <*> newRef Nothing <*> pure f
  return p


runBehaviorM ::  BehaviorM a -> Invalidator -> IO a
runBehaviorM (BehaviorM m) inv = runReaderT m =<< mkWeakPtr inv Nothing

runPull :: PullRef a -> Maybe (Weak Invalidator) -> IO a
runPull ref inv = do
  Pull self invs valRef compute  <- readLazy createPull ref
  traverse_ (modifyRef invs . (:)) inv
  readRef valRef >>= \case
    Nothing -> do
      a <- runBehaviorM compute self
      a <$ writeRef valRef (Just a)
    Just a  -> return a


sampleIO :: Behavior a -> IO a
sampleIO (Constant a) = return a
sampleIO (HoldB h)    = readRef (holdValue h)
sampleIO (PullB ref)  = runPull ref Nothing


sample :: Behavior a -> BehaviorM a
sample (Constant a) = return a
sample (HoldB (Hold invs value sub)) = do
  liftIO (touch sub) --Otherwise the gc seems to know that we never read the IORef again!
  ask >>= modifyRef invs . (:)
  readRef value
sample (PullB ref) = liftIO . runPull ref =<< asks Just


sampleE :: Behavior a -> EventM a
sampleE = liftIO . sampleIO


pattern Invalid :: Height
pattern Invalid = -1

coincidence :: Event (Event a) -> Event a
coincidence Never = Never
coincidence (Event ref) = unsafeCreateEvent (MakeCoincidence ref)

makeCoincidence :: forall a. NodeRef a -> NodeRef (Event a) -> EventM (Node a)
makeCoincidence self ref = do
  parent <- readNodeRef ref
  height <- readHeight parent

  rec
    let c = Coincidence node sub
    sub  <- subscribe parent (CoinOuter c)
    node <- newNode self height (SubscribingCoin c)

  readNode parent >>= traverse_ (connectC c)
  return node


connectC :: Coincidence a -> Event a -> EventM (Maybe a)
connectC _ Never = return Nothing
connectC c@(Coincidence node _) (Event ref) = do
  inner       <- readNodeRef ref
  innerHeight <- readHeight inner
  height      <- readHeight node

  value <- readNode inner
  case value of
    -- Already occured simply pass on the occurance
    Just a  -> writeNode node a

    -- Yet to occur, subscribe the event, record the susbcription as a CoincidenceOcc
    -- and adjust our height
    Nothing -> when (innerHeight >= height) $ do
      sub <- subscribe inner (CoinInner c)
      askModifyRef envCoincidences (CoincidenceOcc sub :)
      liftIO $ propagateHeight innerHeight node
  return value




switch :: Behavior (Event a) -> Event a
switch (Constant e) = e
switch b = unsafeCreateEvent (MakeSwitch b)



makeSwitch :: NodeRef a -> Behavior (Event a) -> EventM (Node a)
makeSwitch self source =  do
  connRef <- newRef Nothing

  rec
    let s   = Switch node connRef inv source
        inv = SwitchInv s
    node <- newNode self 0 (SubscribingSwitch s)

  writeRef (nodeHeight node)  =<< connect s
  return node

connect :: Switch a -> EventM Height
connect s@(Switch node connRef inv source) = do
  e <- liftIO $ runBehaviorM (sample source) inv
  case e of
    Never     -> 0 <$ writeRef connRef Nothing
    Event ref -> do
      parent <- readNodeRef ref
      sub <- subscribe parent (SwitchSub s)

      writeRef connRef (Just sub)
      readNode parent >>= traverse_ (writeNode node)
      readHeight parent


boolM :: Applicative m => Bool -> m b -> m (Maybe b)
boolM True  m  = Just <$> m
boolM False _  = pure Nothing

bool :: Bool -> a -> Maybe a
bool True  a  = Just a
bool False _  = Nothing


reconnect :: Connect -> IO ()
reconnect (Connect s) = do
  -- Forcibly disconnect any existing connection
  conn <- readRef (switchConn s)
  traverse_ unsubscribe conn

  height <- evalEventM $ connect s
  propagateHeight height (switchNode s)



connectSwitches :: [Connect] -> [CoincidenceOcc] -> IO ()
connectSwitches connects coincidences = do
  traverse_ (\(CoincidenceOcc sub) -> unsubscribe sub) coincidences
  traverse_ reconnect connects



push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push _ Never = Never
push f (Event ref) = unsafeCreateEvent $ MakePush ref f

pushAlways :: (a -> EventM b) -> Event a -> Event b
pushAlways f = push (fmap Just . f)


makePush :: NodeRef b -> NodeRef a -> (a -> EventM (Maybe b)) -> EventM (Node b)
makePush self ref f =  do
  parent <- readNodeRef ref
  height <- readHeight parent
  rec

    let p = Push node sub f
    sub <- subscribe parent (PushSub p)
    node <- newNode self height (SubscribingPush p)

  m <- readNode parent
  traverse_ (f >=> traverse_ (writeNode node)) m

  return node


merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge events = case catEvents (DMap.toAscList events) of
  [] -> Never
  refs -> unsafeCreateEvent (MakeMerge refs)


mergeSubscribing :: GCompare k =>  Merge k -> DSum (WrapArg Node k) -> IO (DSum (WrapArg NodeSub k))
mergeSubscribing  m (WrapArg k :=> parent) = do
  sub <- subscribe parent (MergeSub m k)
  return (WrapArg k :=> sub)


makeMerge :: GCompare k => NodeRef (DMap k) -> [DSum (WrapArg NodeRef k)] -> EventM (Node (DMap k))
makeMerge self refs = do
  parents <- traverseDSums readNodeRef refs
  height <- maximumHeight <$> sequence (mapDSums readHeight parents)
  values <- catDSums <$> traverseDSums readNode parents

  rec
    subs   <- liftIO $ traverse (mergeSubscribing m) parents
    m <- Merge node (DMap.fromDistinctAscList subs) <$> newRef DMap.empty
    node <- newNode self (succ height) (SubscribingMerge m)

  when (not  . null  $ values) $ writeNode node (DMap.fromDistinctAscList values)
  return node

never :: Event a
never = Never


fan :: (GCompare k) => Event (DMap k) -> EventSelector k
fan Never = EventSelector $ const Never
fan (Event ref) = EventSelector $ \k -> unsafeCreateEvent (MakeFan k fanRef) where
  {-# NOINLINE fanRef #-}  -- Would be safe, but not efficient/useful if child nodes didn't point at the same fan
  fanRef = unsafeLazy ref


makeFan :: GCompare k => NodeRef (DMap k) -> EventM (Fan k)
makeFan ref = do
  parent <- readNodeRef ref
  rec
    sub  <- subscribe parent (FanSub f)
    f <- Fan sub <$> newRef DMap.empty

  return f


traverseNodes :: forall m k. (MonadIORef m) => (forall a. Node a -> m ()) -> IORef (DMap (WrapArg Node k)) -> m ()
traverseNodes f nodesRef = do
  nodes <- readRef nodesRef
  for_ (DMap.toList nodes) traverseNode'

  where
    traverseNode' :: DSum (WrapArg Node k) -> m ()
    traverseNode' (WrapArg _ :=> node) = f node


lookupFan :: (GCompare k, MonadIORef m) => IORef (DMap (WrapArg Node k)) -> k a -> m (Node a) -> m (Node a)
lookupFan nodesRef k create = do
  nodes     <- readRef nodesRef
  case (DMap.lookup (WrapArg k) nodes) of
    Just node -> return node
    Nothing   -> do
      node  <- create
      writeRef nodesRef (DMap.insert (WrapArg k) node nodes)
      return node


makeFanNode :: GCompare k => NodeRef a -> FanRef k -> k a -> EventM (Node a)
makeFanNode self fanRef k = do
  f@(Fan (NodeSub parent _) nodes) <- readLazy makeFan fanRef
  lookupFan nodes k $ do
    height <- readHeight parent
    node <- newNode self height (SubscribingFan f k)
    readNode parent >>= traverse_ (writeOcc node)
    return node

  where
    writeOcc node occ = traverse_ (writeNode node) (DMap.lookup k occ)


newEventWithFire :: IO (Event a, a -> DSum Trigger)
newEventWithFire = do
  rec
    node  <- newNode nodeRef 0 (SubscribingRoot Nothing)
    nodeRef  <- NodeRef <$> newIORef (Right node)
  return (Event nodeRef, (Trigger node :=>))

newFanEventWithTrigger :: forall k. GCompare k => (forall a. k a -> Trigger a -> IO (IO ())) -> IO (EventSelector k)
newFanEventWithTrigger f = do
  nodesRef <- newRef DMap.empty
  let root = Root nodesRef f
  return $ EventSelector $ \k -> unsafeCreateEvent (MakeRoot k root)

newEventWithTrigger :: forall a. (Trigger a -> IO (IO ())) -> IO (Event a)
newEventWithTrigger f = do
  es <- newFanEventWithTrigger $ \Refl -> f
  return $ select es Refl

makeRoot :: GCompare k => NodeRef a -> k a -> Root k -> EventM (Node a)
makeRoot self k (Root nodes subscr) = liftIO $ lookupFan nodes k $ do
  rec
    node       <- newNode self 0 (SubscribingRoot (Just finalizer))
    finalizer  <- subscr  k (Trigger node)
  return node



-- Be careful to read the subscriber list atomically, in case the actions run on the subscriber
-- result in changes to the subscriber list!
-- In which case we need to be very careful not to lose any new subscriptions

traverseSubs :: MonadIORef m =>  (Subscription a -> m ()) -> Node a -> m ()
traverseSubs f node = do
  subs  <- readRef (nodeSubs node)
  traverse_ f subs

forSubs :: MonadIORef m =>  Node a -> (Subscription a -> m ()) -> m ()
forSubs node f = traverseSubs f node

modifyM :: MonadRef m => Ref m a -> (a -> m a) -> m ()
modifyM ref f = readRef ref >>= f >>= writeRef ref

-- | Delayed operations
delayMerge :: Merge k -> Height ->  EventM ()
delayMerge m height = do
  delayRef <- asks envDelays
  modifyRef delayRef ins
    where ins = IntMap.insertWith (<>) height [DelayMerge m]

delayHold :: Hold a -> a -> EventM ()
delayHold h value = do
  holdsRef <- asks envHolds
  modifyRef holdsRef (WriteHold h value:)


delayInitHold :: Hold a -> EventM ()
delayInitHold ref = do
  holdInitRef <- asks envHoldInits
  modifyRef holdInitRef (HoldInit ref:)


-- | Event propagation
writePropagate ::  Height -> Node a -> a -> EventM ()
writePropagate  height node value = do
  writeNode node value
  propagate height node value



propagate :: forall a. Height -> Node a -> a -> EventM ()
propagate  height node value = traverseSubs propagate' node where

  propagate' :: Subscription a -> EventM ()
  propagate' (PushSub   p) = pushCompute p value >>= traverse_ (writePropagate height (pushNode p))
  propagate' (HoldSub   h) = delayHold h value
  propagate' (SwitchSub s) = writePropagate height (switchNode s) value
  propagate' (CoinInner c) = writePropagate height (coinNode c) value
  propagate' (CoinOuter c) = connectC c value >>= traverse_ (propagate height (coinNode c))

  propagate' (MergeSub m k) = do
    partial <- readRef (mergePartial m)
    writeRef (mergePartial m) $ DMap.insert k value partial
    when (DMap.null partial) $ delayMerge m =<< readHeight (mergeNode m)

  propagate' (FanSub    f) = do
    nodes <- readRef (fanNodes f)
    for_ (DMap.toList value) $ \(k :=> v) ->
      traverse_ (\n -> writePropagate height n v) (DMap.lookup (WrapArg k) nodes)


propagateMerge :: Height -> DelayMerge -> EventM ()
propagateMerge height (DelayMerge m) = do
  height' <- readHeight (mergeNode m)
  -- Check if a coincidence has changed the merge height
  case (height == height') of
    False -> delayMerge m height'
    True -> do
      value <- readRef (mergePartial m)
      writeRef (mergePartial m) (DMap.empty)
      writePropagate height (mergeNode m) value



{-# INLINE takeRef #-}
takeRef :: MonadRef m => Ref m [a] -> m [a]
takeRef ref = readRef ref <* writeRef ref []

{-# INLINE askRef #-}
askRef :: (MonadReader r m, MonadRef m) => (r -> Ref m a) -> m a
askRef = asks >=> readRef

askModifyRef :: (MonadReader r m, MonadRef m) => (r -> Ref m a) -> (a -> a) -> m ()
askModifyRef g f = asks g >>= flip modifyRef f

type InvalidateM a = StateT [Connect] IO a

-- Write holds out and invalidate, return switches to connect!
writeHolds :: [WriteHold] -> IO [Connect]
writeHolds writes = execStateT (traverse_ writeHold writes) []


writeHold :: WriteHold -> InvalidateM ()
writeHold (WriteHold h value) = do
  writeRef (holdValue h) value
  takeRef (holdInvs h) >>= invalidate


invalidate :: [Weak Invalidator] -> InvalidateM ()
invalidate = traverse_ (liftIO . deRefWeak >=> traverse_ invalidate') where
  invalidate' (PullInv p) = do
    writeRef (pullValue p) Nothing
    takeRef (pullInvs p) >>= invalidate

  invalidate' (SwitchInv s) = modify (Connect s:)



-- | Propagate changes in height from a coincidence or switch
-- assumes height as maximum over time
propagateHeight :: Height -> Node a -> IO ()
propagateHeight newHeight node = do
  height <- readHeight node
  when (height < newHeight) $ do
    writeRef (nodeHeight node) newHeight
    forSubs node $ \case
      MergeSub (Merge dest _ _)  _ -> propagateHeight (succ newHeight) dest
      sub -> traverseDest (propagateHeight newHeight) sub

traverseDest :: MonadIORef m => (forall b. Node b -> m ()) -> Subscription a -> m ()
traverseDest f (MergeSub  m _)     = f (mergeNode m)
traverseDest f (PushSub   p)       = f (pushNode p)
traverseDest f (SwitchSub s)       = f (switchNode s)
traverseDest f (CoinInner c)       = f (coinNode c)
traverseDest f (CoinOuter c)       = f (coinNode c)
traverseDest f (FanSub    fan')    = traverseNodes f (fanNodes fan')
traverseDest _ (HoldSub {})        = return ()

maxHeight :: Height -> Height -> Height
maxHeight Invalid _ = Invalid
maxHeight _ Invalid = Invalid
maxHeight h h' = max h h'

maximumHeight :: [Height] -> Height
maximumHeight [] = Invalid
maximumHeight (h:hs) = foldl' maxHeight h hs

isValid :: Height -> Bool
isValid Invalid = False
isValid _       = True

runEventM :: EventM a -> IO (a, Env)
runEventM action = do
  env <- Env <$> newRef mempty <*> newRef [] <*> newRef [] <*> newRef [] <*> newRef []
  (,env) <$> runReaderT (unEventM $ action) env

evalEventM :: EventM a -> IO a
evalEventM = fmap fst . runEventM

execEventM :: EventM a -> IO Env
execEventM = fmap snd . runEventM

runHostFrame :: EventM a -> IO a
runHostFrame action =  evalEventM $ action <* initHolds


-- | Initialize new holds, then check if subscribing new events caused
-- any other new holds to be initialized.
initHolds :: EventM ()
initHolds = do
  newHolds <- takeRef =<< asks envHoldInits
  unless (null newHolds) $ do
    traverse_ initHold newHolds
    initHolds


subscribeEvent :: Event a -> IO (EventHandle a)
subscribeEvent e = evalEventM $ EventHandle <$> eventNode e


takeDelayed :: EventM (Maybe (Height, [DelayMerge]))
takeDelayed = asks envDelays >>= \delaysRef -> liftIO $ do
    delayed   <- readRef delaysRef

    let view = IntMap.minViewWithKey delayed
    traverse_ (writeRef delaysRef) (snd <$> view)
    return (fst <$> view)


endFrame :: Env -> IO ()
endFrame env  = do
  traverse_ clearNode =<< readRef (envClears env)
  connects <- writeHolds =<< readRef (envHolds env)
  connectSwitches connects =<< readRef (envCoincidences env)
  return ()


  where
    clearNode (SomeNode node) = writeRef (nodeOcc node) Nothing

fireEventsAndRead :: [DSum Trigger] -> EventM a -> IO a
fireEventsAndRead triggers runRead = do
  (a, env) <- runEventM $ do
    traverse_ propagateRoot triggers
    runDelays
    runRead <* initHolds

  a <$ endFrame env

  where
    runDelays = takeDelayed >>= traverse_  (\(height, merges) -> do
        traverse_ (propagateMerge height) merges
        runDelays)

    propagateRoot (Trigger node :=> a) = writePropagate 0 node a

fireEvents :: [DSum Trigger] -> IO ()
fireEvents triggers = fireEventsAndRead triggers (return ())


data Ant

instance R.Reflex Ant where
  newtype Behavior Ant a = AntBehavior { unAntBehavior :: Behavior a }
  newtype Event Ant a = AntEvent { unAntEvent :: Event a }
  type PullM Ant = BehaviorM
  type PushM Ant = EventM
  {-# INLINE never #-}
  {-# INLINE constant #-}
  never = AntEvent never
  constant = AntBehavior . constant
  push f = AntEvent. push f . unAntEvent
  pull = AntBehavior . pull
  merge = AntEvent . merge . (unsafeCoerce :: DMap (WrapArg (R.Event Ant) k) -> DMap (WrapArg Event k))
  fan e = R.EventSelector $ AntEvent . select (fan (unAntEvent e))
  switch = AntEvent . switch . (unsafeCoerce :: Behavior (R.Event Ant a) -> Behavior (Event a)) . unAntBehavior
  coincidence = AntEvent . coincidence . (unsafeCoerce :: Event (R.Event Ant a) -> Event (Event a)) . unAntEvent


--HostFrame instances
instance R.MonadSubscribeEvent Ant EventM where
  subscribeEvent (AntEvent e) = liftIO $ subscribeEvent e

instance R.MonadReflexCreateTrigger Ant EventM where
  newEventWithTrigger    f = liftIO $ AntEvent <$> newEventWithTrigger f
  newFanEventWithTrigger f = liftIO $ do
    es <- newFanEventWithTrigger f
    return $ R.EventSelector $ AntEvent . select es

instance R.MonadSample Ant BehaviorM where
  sample (AntBehavior b) = sample b

instance R.MonadSample Ant EventM where
  sample (AntBehavior b) = sampleE b

instance R.MonadHold Ant EventM where
  hold a (AntEvent e) = AntBehavior <$> hold a e


newtype AntHost a = AntHost { runAntHost :: IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

--Host instances
instance R.MonadReflexCreateTrigger Ant AntHost where
  newEventWithTrigger    f = liftIO $ AntEvent <$> newEventWithTrigger f
  newFanEventWithTrigger f = liftIO $ do
    es <- newFanEventWithTrigger f
    return $ R.EventSelector $ AntEvent . select es

instance R.MonadSubscribeEvent Ant AntHost where
  subscribeEvent (AntEvent e) = liftIO $ subscribeEvent e

instance R.MonadReadEvent Ant EventM where
  {-# INLINE readEvent #-}
  readEvent h = fmap return <$> liftIO (readEvent h)

instance R.MonadReflexHost Ant AntHost where
  type ReadPhase AntHost = EventM

  fireEventsAndRead es = liftIO . fireEventsAndRead es
  runHostFrame = liftIO . runHostFrame

instance R.MonadSample Ant AntHost where
  sample (AntBehavior b) = liftIO (sampleIO b)


instance R.ReflexHost Ant where
  type EventTrigger Ant = Trigger
  type EventHandle Ant = EventHandle
  type HostFrame Ant = EventM


instance MonadRef AntHost where
  type Ref AntHost = Ref IO
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a


-- DMap utilities
catEvents ::  [DSum (WrapArg Event k)] -> [DSum (WrapArg NodeRef k)]
catEvents events = [(WrapArg k) :=> ref | (WrapArg k) :=> Event ref <- events]

traverseDSums :: Applicative m => (forall a. f a -> m (g a)) -> [DSum (WrapArg f k)] -> m [DSum (WrapArg g k)]
traverseDSums f = traverse (\(WrapArg k :=> v) -> (WrapArg k :=>) <$> f v)

mapDSums :: (forall a. f a -> b) -> [DSum (WrapArg f k)] -> [b]
mapDSums f = map (\(WrapArg _ :=> v) -> f v)


mapDMap :: (forall a. f a -> b) -> DMap (WrapArg f k) -> [b]
mapDMap f = mapDSums f . DMap.toList


catDSums :: [DSum (WrapArg Maybe k)] -> [DSum k]
catDSums = catMaybes . map toMaybe

toMaybe :: DSum (WrapArg Maybe k)  -> Maybe (DSum k)
toMaybe (WrapArg k :=> Just v ) = Just (k :=> v)
toMaybe _ = Nothing

