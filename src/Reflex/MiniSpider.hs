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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Reflex.MiniSpider where



import Data.IORef
import System.Mem.Weak

import System.IO.Unsafe
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.Primitive


import Data.Foldable
import Data.Maybe

import Data.Functor.Misc

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap, GCompare)

import Data.Dependent.Sum

import Data.Semigroup

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data SomeNode = forall a. SomeNode { unSome :: Node a }



data MakeNode a where
  MakeNode  :: !(NodeRef a) -> !(a -> EventM (Maybe b)) -> MakeNode b
  MakeMerge :: GCompare k => !([DSum (WrapArg NodeRef k)]) -> MakeNode (DMap k)
  MakeSwitch :: !(Behavior (Event a)) -> MakeNode a
  MakeRoot  :: MakeNode a


type LazyRef a b = IORef (Either a b)
newtype NodeRef a = NodeRef (LazyRef (MakeNode a) (Node a))

data Event a
    = Never
    | Event !(NodeRef a)


type PullRef a = LazyRef (BehaviorM a) (Pull a)

data Behavior a
    = Constant !a
    | PullB !(PullRef a)
    | HoldB !(Hold a)


newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int

data Subscription a b where
  PushSub     :: Node a -> Node b -> (a -> EventM (Maybe b)) -> Subscription a b
  MergeSub    :: GCompare k => Merge k -> k a -> Subscription a (DMap k)
  HoldSub     :: Node a -> Hold a -> Subscription a a
  SwitchSub   :: Switch a -> Subscription a a

instance Show (Subscription a b) where
  show (PushSub {}) = "Push"
  show (MergeSub {}) = "Merge"
  show (HoldSub {}) = "Hold"
  show (SwitchSub {}) = "Hold"


data Subscribing b where
  Subscribing       :: (Subscription a b) -> Subscribing b
  SubscribingMerge  :: (Merge k) -> Subscribing (DMap k)
  SubscribingSwitch :: (Switch a) -> Subscribing a
  SubscribingRoot   :: Subscribing b

data WeakSubscriber a = forall b. WeakSubscriber { unWeak :: !(Weak (Subscription a b)) }


data Node a = Node
  { nodeSubs      :: !(IORef [WeakSubscriber a])
  , nodeHeight    :: !(IORef Int)
  , nodeParents   :: (Subscribing a)
  , nodeValue     :: !(IORef (Maybe a))
  }


data Invalidator where
  PullInv   :: Pull a   -> Invalidator
  SwitchInv :: Switch a -> Invalidator


data Pull a  = Pull
  { pullInv   :: Invalidator
  , pullInvs  :: !(IORef [Weak Invalidator])
  , pullValue :: !(IORef (Maybe a))
  , pullCompute :: (BehaviorM a)
  }

data Hold a = Hold
  { holdInvs     :: !(IORef [Weak Invalidator])
  , holdValue    :: !(IORef a)
  , holdSub      :: !(LazyRef (Event a) (Maybe (Subscription a a)))
  }

type NodeSub a = (Node a, Weak (Subscription a a))

data Switch a = Switch
  { switchSub    :: Subscription a a
  , switchConn   :: !(IORef (Maybe (NodeSub a)))
  , switchNode   :: Node a
  , switchInv    :: Invalidator
  , switchSource :: Behavior (Event a)
  }


data MergeParent k a where
  MergeParent      :: Node a -> (Subscription a (DMap k)) -> MergeParent k a

data Merge k = Merge
  { mergeNode     :: Node (DMap k)
  , mergeParents  :: DMap (WrapArg (MergeParent k) k)
  , mergePartial  :: !(IORef (DMap k))
  }


data WriteHold where
  WriteHold :: !(Hold a) -> !a -> WriteHold

data HoldInit where
  HoldInit :: !(Hold a) -> HoldInit

data Connect where
  Connect :: !(Switch a) -> Connect

data DelayMerge where
  DelayMerge :: !(Merge k) -> DelayMerge


data Env = Env
  { envDelays :: !(IORef (IntMap [DelayMerge]))
  , envClears :: !(IORef [SomeNode])
  , envHolds  :: !(IORef [WriteHold])
  , envHoldInits  :: !(IORef [HoldInit])
  }

newtype EventM a = EventM { unEventM :: ReaderT Env IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadReader Env)

newtype BehaviorM a = BehaviorM { unBehaviorM :: ReaderT (Weak Invalidator) IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadReader (Weak Invalidator))


instance MonadRef EventM where
  type Ref EventM = Ref IO
  {-# INLINE newRef #-}
  {-# INLINE readRef #-}
  {-# INLINE writeRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a

instance MonadRef BehaviorM where
  type Ref BehaviorM = Ref IO
  {-# INLINE newRef #-}
  {-# INLINE readRef #-}
  {-# INLINE writeRef #-}
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



type MonadIORef m = (MonadIO m, MonadRef m, Ref m ~ IORef)

{-# SPECIALIZE readLazy :: (a -> IO b) -> LazyRef a b -> IO b #-}
readLazy :: MonadIORef m => (a -> m b) -> LazyRef a b -> m b
readLazy create ref = readRef ref >>= \case
    Left a -> do
      node <- create a
      writeRef ref (Right node)
      return node
    Right node -> return node

readNodeRef :: NodeRef a -> EventM (Node a)
readNodeRef (NodeRef ref) = readLazy createNode ref


eventNode :: Event a -> EventM (Maybe (Node a))
eventNode Never       = return Nothing
eventNode (Event ref) = Just <$> readNodeRef ref


readHeight :: MonadIORef m => Node a -> m Int
readHeight node = readRef (nodeHeight node)


newNode :: MonadIO m => Height -> Subscribing a -> m (Node a)
newNode height parents = liftIO $
  Node <$> newRef [] <*> newRef height <*> pure parents <*> newRef Nothing


readNode :: Node a -> EventM (Maybe a)
readNode node = readRef (nodeValue node)


writeNode :: Node a -> a -> EventM ()
writeNode node a = do
  writeRef (nodeValue node) (Just a)
  clearsRef <- asks envClears
  modifyRef clearsRef (SomeNode node :)

readEvent :: EventHandle a -> EventM (Maybe a)
readEvent (EventHandle e) = fmap join <$> traverse readNode =<< eventNode e


subscribe :: MonadIORef m => Node a -> Subscription a b -> m (Weak (Subscription a b))
subscribe node sub = do
  weakSub <- liftIO (mkWeakPtr sub Nothing)
  modifyRef (nodeSubs node) (WeakSubscriber weakSub :)
  return weakSub

subscribe_ :: MonadIORef m => Node a -> Subscription a b -> m ()
subscribe_ node = void . subscribe node

createNode :: MakeNode a -> EventM (Node a)
createNode (MakeNode ref f) = makePush ref f
createNode (MakeMerge refs) = makeMerge refs
createNode (MakeSwitch b) = makeSwitch b
createNode MakeRoot = newNode 0 SubscribingRoot

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

    let sub = HoldSub parent h
    subscribe_ parent sub

    return (Just sub)


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

switch :: Behavior (Event a) -> Event a
switch (Constant e) = e
switch b = unsafeCreateEvent (MakeSwitch b)

makeSwitch ::  Behavior (Event a) -> EventM (Node a)
makeSwitch source =  do
  connRef <- newRef Nothing
  rec
    let s   = Switch sub connRef node inv source
        inv = SwitchInv s
        sub = SwitchSub s
    node <- newNode 0 (SubscribingSwitch s)

  writeRef (nodeHeight node)  =<< connect s
  return node

connect :: Switch a -> EventM Height
connect (Switch sub connRef node inv source) = do
  e <- liftIO $ runBehaviorM (sample source) inv
  case e of
    Never       -> 0 <$ writeRef connRef Nothing
    Event ref -> do
      parent <- readNodeRef ref
      weakSub <- subscribe parent sub

      writeRef connRef (Just (parent, weakSub))
      readNode parent >>= traverse_ (writeNode node)
      readHeight parent


whenM :: Applicative m => Bool -> m b -> m (Maybe b)
whenM True  m  = Just <$> m
whenM False _  = pure Nothing

bool :: Bool -> a -> Maybe a
bool True  a  = Just a
bool False _  = Nothing


reconnect :: Connect -> EventM (Maybe (SomeNode, Height))
reconnect (Connect s) = do
  -- Forcibly disconnect any existing connection
  conn <- readRef (switchConn s)
  liftIO (traverse_ (finalize . snd) conn)

  new <- connect s
  liftIO $ do
    old <- readHeight node
    whenM (new /= old) $ do
      invalidateHeight new node
      return (SomeNode node, new)

    where
      node = switchNode s


connectSwitches :: [Connect] -> EventM ()
connectSwitches connects = do
  recomputes <- traverse reconnect connects
  liftIO $ for_ (catMaybes recomputes) $ \(SomeNode n, height) ->
    recomputeInvalid height n



push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push _ Never = Never
push f (Event ref) = unsafeCreateEvent $ MakeNode ref f

pushAlways :: (a -> EventM b) -> Event a -> Event b
pushAlways f = push (fmap Just . f)


makePush :: NodeRef a -> (a -> EventM (Maybe b)) -> EventM (Node b)
makePush ref f =  do
  parent <- readNodeRef ref
  height <- readHeight parent
  rec
    let sub = PushSub parent node f
    node <- newNode height (Subscribing sub)

  readNode parent >>= traverse_
    (f >=> traverse_ (writeNode node))

  subscribe_ parent sub
  return node


merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge events = case catEvents (DMap.toAscList events) of
  [] -> Never
  refs -> unsafeCreateEvent (MakeMerge refs)


mergeSubscribing :: GCompare k =>  Merge k -> DSum (WrapArg Node k) -> IO (DSum (WrapArg (MergeParent k) k))
mergeSubscribing  merge' (WrapArg k :=> parent) = do
  subscribe_ parent sub
  return (WrapArg k :=> MergeParent parent sub)

  where sub = MergeSub merge' k


makeMerge :: GCompare k =>  [DSum (WrapArg NodeRef k)] -> EventM (Node (DMap k))
makeMerge refs = do
  parents <- traverseDSums readNodeRef refs
  height <- maximumHeight <$> sequence (mapDSums readHeight parents)
  values <- catDSums <$> traverseDSums readNode parents

  rec
    subs   <- liftIO $ traverse (mergeSubscribing merge') parents
    merge' <- Merge node (fromAsc subs) <$> newRef DMap.empty
    node <- newNode (succ height) (SubscribingMerge merge')

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



traverseWeak :: MonadIORef m => (forall b. Subscription a b -> m ()) -> [WeakSubscriber a] -> m [WeakSubscriber a]
traverseWeak f subs = do
  flip filterM subs $ \(WeakSubscriber weak) -> do
    m <- liftIO (deRefWeak weak)
    isJust m <$ traverse_ f m

traverseSubs :: MonadIORef m =>  (forall b. Subscription a b -> m ()) -> Node a -> m ()
traverseSubs f node = modifyM (nodeSubs node) $ traverseWeak f


forSubs :: MonadIORef m =>  Node a -> (forall b. Subscription a b -> m ()) -> m ()
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

  propagate' :: Subscription a b -> EventM ()
  propagate' (PushSub _ dest f)  =
    f value >>= traverse_ (writePropagate height dest)

  propagate' (MergeSub m k) = do
    partial <- readRef (mergePartial m)
    writeRef (mergePartial m) $ DMap.insert k value partial
    when (DMap.null partial) $ delayMerge m =<< readHeight (mergeNode m)

  propagate' (HoldSub _ h) =  delayHold h value

  propagate' (SwitchSub s) = writePropagate height (switchNode s) value


propagateMerge :: Height -> DelayMerge -> EventM ()
propagateMerge height (DelayMerge m) = do
  value <- readRef (mergePartial m)
  writeRef (mergePartial m) (DMap.empty)
  writePropagate height (mergeNode m) value



{-# INLINE readClear #-}
readClear :: MonadRef m => Ref m [a] -> m [a]
readClear ref = readRef ref <* writeRef ref []

{-# INLINE askRef #-}
askRef :: (MonadReader r m, MonadRef m) => (r -> Ref m a) -> m a
askRef = asks >=> readRef


type InvalidateM a = StateT [Connect] IO a

-- Write holds out and invalidate, return switches to connect!
writeHolds :: [WriteHold] -> IO [Connect]
writeHolds writes = execStateT (traverse_ writeHold writes) []


writeHold :: WriteHold -> InvalidateM ()
writeHold (WriteHold h value) = do
  writeRef (holdValue h) value
  readClear (holdInvs h) >>= invalidate


invalidate :: [Weak Invalidator] -> InvalidateM ()
invalidate = traverse_ (liftIO . deRefWeak >=> traverse_ invalidate') where
  invalidate' (PullInv p) = do
    writeRef (pullValue p) Nothing
    readClear (pullInvs p) >>= invalidate

  invalidate' (SwitchInv s) = modify (Connect s:)



-- | Recompute heights from switch connects in two stages
-- to save computing the same heights repeatedly
invalidateHeight :: Height -> Node a ->  IO ()
invalidateHeight newHeight node  = do
  height <- readHeight node
  when (height /= Invalid) $ do
    writeRef (nodeHeight node) Invalid

    forSubs node $ \case
      MergeSub (Merge dest _ _)  _ -> do
        height' <- readHeight dest
        when (height' == height + 1 || height' < newHeight + 1) $ do
          invalidateHeight (newHeight + 1) dest
      sub -> traverseDest (invalidateHeight newHeight) sub


recomputeInvalid :: Height -> Node a -> IO ()
recomputeInvalid newHeight node = do
  height <- readHeight node
  when (height == Invalid) $ do
    writeRef (nodeHeight node) newHeight
    forSubs node $ \case
      MergeSub (Merge dest parents _)  _ -> do
        mergeHeight <- maximumHeight <$> sequence (mapDMap (\(MergeParent p _) -> readHeight p) parents)
        when (isValid mergeHeight) $ recomputeInvalid (succ mergeHeight) dest
      sub -> traverseDest (recomputeInvalid newHeight) sub


--recomputeHeight ::

traverseDest :: Monad m => (Node b -> m ()) -> Subscription a b -> m ()
traverseDest f (MergeSub  m _)    = f (mergeNode m)
traverseDest f (PushSub  _ node _) = f node
traverseDest f (SwitchSub s)       = f (switchNode s)
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

runEventM :: EventM a -> IO a
runEventM action = do
  env <- Env <$> newRef mempty <*> newRef [] <*> newRef [] <*> newRef []
  runReaderT (unEventM $ action) env


runHostFrame :: EventM a -> IO a
runHostFrame action = runEventM $ do
  a <- action
  endFrame (return a)


-- | Initialize new holds, then check if subscribing new events caused
-- any other new holds to be initialized.
initHolds :: EventM ()
initHolds = do
  newHolds <- readClear =<< asks envHoldInits
  unless (null newHolds) $ do
    traverse_ initHold newHolds
    initHolds



endFrame :: EventM a -> EventM a
endFrame readEvents = do
  initHolds
  a <- readEvents -- Read events before this frame's events are cleared

  liftIO . traverse_ clearNode =<< askRef envClears
  connects <- liftIO . writeHolds =<< askRef  envHolds

  connectSwitches connects
  return a

  where
    clearNode (SomeNode node) = writeRef (nodeValue node) Nothing



subscribeEvent :: Event a -> IO (EventHandle a)
subscribeEvent e = runEventM $ do
  void (eventNode e)
  return (EventHandle e)



takeDelayed :: EventM (Maybe (Height, [DelayMerge]))
takeDelayed = do
  delaysRef <- asks envDelays
  delayed   <- readRef delaysRef

  let view = IntMap.minViewWithKey delayed
  traverse_ (writeRef delaysRef) (snd <$> view)
  return (fst <$> view)


fireEventsAndRead :: [Trigger] -> EventM a -> IO a
fireEventsAndRead triggers runRead = runEventM $ do

  traverse_ propagateRoot triggers
  runDelays
  endFrame runRead

  where
    runDelays = takeDelayed >>= traverse_  (\(height, merges) -> do
        traverse_ (propagateMerge height) merges
        runDelays)

    propagateRoot (Trigger nodeRef a) = do
      node <- readNodeRef nodeRef
      writePropagate 0 node a

fireEvents :: [Trigger] -> IO ()
fireEvents triggers = fireEventsAndRead triggers (return ())






-- prop_factor :: [Word8] -> Property
-- prop_factor (Positive n) = monadicIO $ do
--   factors <- run $ do
--
--   assert (product factors == n)
--     where




-- main = do
-- --   for_ [1..4] $ \_ ->
--     pullTree 200 2

-- main = do
--   (bs, fires) <- holdCounters 10000
--   let b = mergeTree 8 bs
--
--   total <- for [1..10] $ const $ do
--     firing <- generate (sublistOf fires)
--     fireEvents (zipWith ($) firing (repeat 1))
--
--     x <- sampleIO b
--     print (x, length firing)
--
--     return (length firing)
--   return ()
--
--   x <- sampleIO $ pull (sum <$> traverse sample bs)
--   print (x, sum total)


--   sample n >>= print


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

instance Functor Event where
  fmap f e = push (return .   Just . f) e

instance Functor Behavior where
  fmap f b = pull (f <$> sample b)


instance (Monoid a) => Monoid (Behavior a) where
  mempty = constant mempty
  mappend a b = pull $ liftM2 mappend (sample a) (sample b)
  mconcat = pull . liftM mconcat . mapM sample

instance (Semigroup a) => Monoid (Event a) where
  mempty = never
  mappend a b = mconcat [a, b]
  mconcat = fmap sconcat . mergeList


tag :: Behavior b -> Event a -> Event b
tag b = pushAlways $ \_ -> sampleE b

attachWith :: (a -> b -> c) -> Behavior a -> Event b -> Event c
attachWith f b = pushAlways $ \x -> flip f x <$> sampleE b

counter :: Event a -> EventM (Event Int)
counter e = mdo
  n <- hold 0 next

  let next = attachWith (+) n (1 <$ e)
  return next



foldDyn :: (a -> b -> b) -> b -> Event a -> EventM (Event b, Behavior b)
foldDyn f initial e = do
  rec
    let e' = flip pushAlways e $ \a -> f a <$> sampleE b
    b <- hold initial e'

  return (e', b)


headE ::  Event  a -> EventM (Event a)
headE e = do
  rec be <- hold e $ fmap (const never) e'
      let e' = switch be
      e' `seq` return ()
  return e'



eventDMap :: [Event a] -> DMap (WrapArg Event (Const2 Int a))
eventDMap es = fromAsc $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es

fromDMap :: DMap (Const2 Int a) -> [a]
fromDMap = map (\(Const2 _ :=> v) -> v) . DMap.toList

mergeWith :: (a -> a -> a) -> [Event a] -> Event a
mergeWith f es =  foldl1 f <$> mergeList es

mergeList ::  [Event a] -> Event (NonEmpty a)
mergeList es = NE.fromList . fromDMap <$> merge (eventDMap es)

leftmost :: [Event a] -> Event a
leftmost = mergeWith const

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

fromAsc :: [DSum k] -> DMap k
fromAsc = DMap.fromDistinctAscList
