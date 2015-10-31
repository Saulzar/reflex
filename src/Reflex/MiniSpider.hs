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

module Reflex.MiniSpider where



import Data.IORef
import System.Mem.Weak
import System.Mem

import System.IO.Unsafe
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.State.Strict

import Control.Monad.IO.Class
import Control.Monad.Primitive


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

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

data SomeNode = forall a. SomeNode { unSome :: Node a }



data MakeNode a where
  MakeNode  :: NodeRef a -> (a -> EventM (Maybe b)) -> MakeNode b
  MakeMerge :: GCompare k => [DSum (WrapArg NodeRef k)] -> MakeNode (DMap k)
  MakeSwitch :: Behavior (Event a) -> MakeNode a
  MakeRoot  :: MakeNode a

 
type LazyRef a b = IORef (Either a b)
newtype NodeRef a = NodeRef (LazyRef (MakeNode a) (Node a))

data Event a 
    = Never 
    | Event !(NodeRef a)
    
    
type PullRef a = LazyRef (BehaviorM a) (Pull a)
    
data Behavior a 
    = Constant a 
    | PullB (PullRef a)
    | HoldB (Hold a)


newtype EventHandle a = EventHandle { unEventHandle :: Event a }

type Height = Int

data Subscription a b where
  PushSub     :: Node a -> Node b -> (a -> EventM (Maybe b)) -> Subscription a b
  MergeSub    :: GCompare k => Node a -> Node (DMap k) -> IORef (DMap k) -> k a -> Subscription a (DMap k)
  HoldSub     :: Node a -> Hold a -> Subscription a a
  SwitchSub   :: Switch a -> Subscription a a

instance Show (Subscription a b) where
  show (PushSub {}) = "Push"
  show (MergeSub {}) = "Merge"
  show (HoldSub {}) = "Hold"
  show (SwitchSub {}) = "Hold"



data Subscribing b = forall a. Subscribing { unSubscribing :: !(Subscription a b) }
data WeakSubscriber a = forall b. WeakSubscriber { unWeak :: !(Weak (Subscription a b)) }


data Node a = Node 
  { nodeSubs      :: !(IORef [WeakSubscriber a])
  , nodeHeight    :: !(IORef Int)  
  , nodeParents   :: [Subscribing a]
  , nodeValue     :: !(IORef (Maybe a))
  }

  
data Invalidator where
  PullInv   :: Pull a   -> Invalidator
  SwitchInv :: Switch a -> Invalidator

  
data Pull a  = Pull 
  { pullInv   :: !Invalidator 
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
  

data WriteHold where 
  WriteHold :: Hold a -> a -> WriteHold
  
data HoldInit where
  HoldInit :: Hold a -> HoldInit
  
data Connect where
  Connect :: Switch a -> Connect
  
data DelayMerge where
  DelayMerge :: Node (DMap k) -> IORef (DMap k) -> DelayMerge
  

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

readLazy :: MonadIORef m => (a -> m b) -> LazyRef a b -> m b
readLazy create ref = readRef ref >>= \case
    Left init -> do
      node <- create init
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



newNode :: Height -> [Subscribing a] -> EventM (Node a)
newNode height parents = 
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


subscribe :: Node a -> Subscription a b -> EventM (Weak (Subscription a b))
subscribe node sub = do
  weakSub <- liftIO (mkWeakPtr sub Nothing)
  modifyRef (nodeSubs node) (WeakSubscriber weakSub :)
  return weakSub
  

createNode :: MakeNode a -> EventM (Node a)
createNode (MakeNode ref f) = makePush ref f
createNode (MakeMerge refs) = makeMerge refs 
createNode (MakeSwitch b) = makeSwitch b 
createNode MakeRoot = newNode 0 []

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
    subscribe parent sub
    
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

  
invalid :: Height
invalid = -1

switch :: Behavior (Event a) -> Event a
switch (Constant e) = e
switch b = unsafeCreateEvent (MakeSwitch b)

makeSwitch :: Behavior (Event a) -> EventM (Node a)
makeSwitch source =  do 
  connRef <- newRef Nothing
  rec
    let s   = Switch sub connRef node inv source
        inv = SwitchInv s
        sub = SwitchSub s
    node <- newNode 0 [Subscribing sub]
  
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
    

reconnect :: Connect -> EventM (SomeNode, Height)
reconnect (Connect s) = do
  -- Forcibly disconnect any existing connection
  conn <- readRef (switchConn s)
  liftIO (traverse_ (finalize . snd) conn)
  (SomeNode (switchNode s), ) <$> connect s
  

connectSwitches :: [Connect] -> EventM ()
connectSwitches connects = do
  traverse reconnect connects 
    
  return ()

  

push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f Never = Never
push f (Event ref) = unsafeCreateEvent $ MakeNode ref f

pushAlways :: (a -> EventM b) -> Event a -> Event b
pushAlways f = push (fmap Just . f)


makePush :: NodeRef a -> (a -> EventM (Maybe b)) -> EventM (Node b)
makePush ref f =  do 
  parent <- readNodeRef ref
  height <- readHeight parent
  rec
    let sub = PushSub parent node f
    node <- newNode height [Subscribing sub]
  
  readNode parent >>= traverse_ 
    (f >=> traverse_ (writeNode node))
  
  subscribe parent sub
  return node  
  

merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge events = case catEvents (DMap.toAscList events) of
  [] -> Never
  refs -> unsafeCreateEvent (MakeMerge refs) 


mergeSubscribing :: GCompare k => Node (DMap k) -> IORef (DMap k) -> DSum (WrapArg Node k) -> EventM (Subscribing (DMap k))  
mergeSubscribing node partial (WrapArg k :=> parent) = do
  subscribe parent sub
  return (Subscribing sub)
  
  where sub = MergeSub parent node partial k

  
makeMerge :: GCompare k => [DSum (WrapArg NodeRef k)] -> EventM (Node (DMap k))
makeMerge refs = do     
  parents <- traverseDSums readNodeRef refs
  height <- maximum <$> sequence (mapDSums readHeight parents) 
  values <- catDSums <$> traverseDSums readNode parents
  partial <- newRef DMap.empty
  
  rec
    subs <- traverse (mergeSubscribing node partial) parents
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

    

traverseWeak :: MonadIORef m => (forall b. Subscription a b -> m ()) -> [WeakSubscriber a] -> m [WeakSubscriber a]
traverseWeak f subs = do
  flip filterM subs $ \(WeakSubscriber weak) -> do 
    m <- liftIO (deRefWeak weak)
    isJust m <$ traverse_ f m 

traverseSubs :: MonadIORef m =>  (forall b. Subscription a b -> m ()) -> Node a -> m ()
traverseSubs f node = modifyM  (nodeSubs node) $ traverseWeak f

modifyM :: MonadRef m => Ref m a -> (a -> m a) -> m ()
modifyM ref f = readRef ref >>= f >>= writeRef ref
    
    
    
-- | Delayed operations    
delayMerge :: Node (DMap k) -> IORef (DMap k) -> Height ->  EventM ()    
delayMerge node valueRef height = do
  delayRef <- asks envDelays
  modifyRef delayRef insert
    where insert = IntMap.insertWith (<>) height [DelayMerge node valueRef]
          
          
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

  propagate' (MergeSub _ dest partial k) = do
    m <- readRef partial
    writeRef partial $ DMap.insert k value m
    when (DMap.null m) $ delayMerge dest partial =<< readHeight dest
      
  propagate' (HoldSub _ h) =  delayHold h value
  
  propagate' (SwitchSub s) = writePropagate height (switchNode s) value

  
propagateMerge :: Height -> DelayMerge -> EventM ()
propagateMerge height (DelayMerge node valueRef) = do
  value <- readRef valueRef
  writeRef valueRef (DMap.empty)
  writePropagate height node value
  

  
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
  invs <- readRef (holdInvs h)
  readClear (holdInvs h) >>= invalidate

  
invalidate :: [Weak Invalidator] -> InvalidateM ()
invalidate = traverse_ (liftIO . deRefWeak >=> traverse_ invalidate') where
  invalidate' (PullInv p) = do
    writeRef (pullValue p) Nothing
    readClear (pullInvs p) >>= invalidate

  invalidate' (SwitchInv s) = modify (Connect s:)


invalidateHeight :: Node a ->  IO ()
invalidateHeight node  = do
  height <- readHeight node
  when (height /= invalid) $ do
    writeRef (nodeHeight node) invalid
    traverseSubs invalidateSub node
   
    where
      invalidateSub :: Subscription a b -> IO ()
      invalidateSub sub = traverse_ invalidateHeight (snd <$> dest sub)

      
-- | Give the destination of a subscription, and difference in height
dest :: Subscription a b -> [(Height, Node b)]
dest (MergeSub _ node _ _) = [(1, node)]
dest (PushSub  _ node _)   = [(0, node)]
dest (SwitchSub s)         = [(0, switchNode s)]
dest HoldSub {}            = []
   
   
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
  inits <- readClear =<< asks envHoldInits  
  unless (null inits) $ do
    traverse_ initHold inits
    initHolds
  

  
endFrame :: EventM a -> EventM a
endFrame readEvents = do  
  initHolds
  a <- readEvents -- Read events before this frame's events are cleared
  
  liftIO . traverse_ clearNode =<< askRef envClears 
  connects <- liftIO . writeHolds =<< askRef  envHolds
  
--   connectSwitches connects
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
  delayed <- readRef delaysRef
  
  let view = IntMap.minViewWithKey delayed
  traverse_ (writeRef delaysRef) (snd <$> view) 
  return (fst <$> view)

  
fireEventsAndRead :: [Trigger] -> EventM a -> IO a
fireEventsAndRead triggers runRead = runEventM $ do

  roots <- traverse propagateRoot triggers
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
-- 
--   assert (product factors == n)
--   
--     where

-- main = do
-- 
--   (input1, fire1) <- newEventWithFire
--   (input2, fire2) <- newEventWithFire 
--   
--   let out = mergeList [input1, input2, (+1) <$> input1, (+2) <$> input2]
-- 
--   n <- runHostFrame $ do 
--     count <- counter out >>= counter
--     headE $ sum <$> mergeList [count, input2]
--   
--   
--   handle <- subscribeEvent n
--   x <- fireEventsAndRead [fire1 (3::Int), fire2 (2::Int)] (readEvent handle)
--   print x
-- 
--   
--   x <- fireEventsAndRead [fire1 (3::Int)] (readEvent handle)
--   print x
-- 
--   
--   x <- fireEventsAndRead [fire1 (3::Int)] (readEvent handle)
--   print x
  
  
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