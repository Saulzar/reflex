{-# LANGUAGE FunctionalDependencies, BangPatterns, UndecidableInstances, ConstraintKinds, GADTs, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, RecursiveDo, FlexibleContexts, StandaloneDeriving #-}
module Reflex.Test.Plan
  ( TestPlan(..)
  , Readable
  , runPlan
  , testPlan
  , Plan

  , subscribePlan

  , execSchedule
  , readSchedule


  ) where

import Reflex.Class
import Reflex.Host.Class

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import Data.Dependent.Sum (DSum (..))
import Data.Monoid
import Data.Maybe
import qualified Data.IntMap as IntMap
import Control.Monad.Ref
import Control.DeepSeq (NFData (..))


import Data.IntMap
import Data.IORef

-- Note: this import must come last to silence warnings from AMP
import Prelude

type MonadIORef m = (MonadRef m, Ref m ~ Ref IO)

class (Reflex t, MonadHold t m, MonadFix m) => TestPlan t m where
  -- | Speicify a plan of an input Event firing
  -- Occurances must be in the future (i.e. Time > 0)
  -- Initial specification is

  plan :: [(Word, a)] -> m (Event t a)



data Firing t where
  Firing :: IORef (Maybe (EventTrigger t a)) -> a -> Firing t


instance NFData (Behavior t a) where
  rnf !_ = ()

instance NFData (Event t a) where
  rnf !_ = ()

instance NFData (Firing t) where
  rnf !_ = ()

readEvent' :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
readEvent' = readEvent >=> sequence


class MonadReflexHost t m => Readable t m a r | a -> r where
  getRead ::  a -> m (ReadPhase m r)

instance MonadReflexHost t m => Readable t m (Event t a) (Maybe a) where
  getRead e = do
    handle <- subscribeEvent e
    return (readEvent' handle)

instance MonadReflexHost t m => Readable t m (Behavior t a) a where
  getRead b = return (sample b)

instance (MonadReflexHost t m, Readable t m a r, Readable t m b s) => Readable t m (a, b) (r, s) where
  getRead (a, b) = do
    readA <- getRead a
    readB <- getRead b
    return (liftA2 (,) readA readB)

type Schedule t = IntMap [Firing t]
-- Implementation of a TestPlan
newtype Plan t a = Plan (StateT (Schedule t) (HostFrame t) a)

deriving instance ReflexHost t => Functor (Plan t)
deriving instance ReflexHost t => Applicative (Plan t)
deriving instance ReflexHost t => Monad (Plan t)

deriving instance ReflexHost t => MonadSample t (Plan t)
deriving instance ReflexHost t => MonadHold t (Plan t)
deriving instance ReflexHost t => MonadFix (Plan t)


instance (ReflexHost t, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => TestPlan t (Plan t) where
  plan occurances = Plan $ do
    (e, ref) <- newEventWithTriggerRef
    modify (IntMap.unionWith mappend (firings ref))
    return e

    where
      firings ref = IntMap.fromList (makeFiring ref <$> occurances)
      makeFiring ref (t, a) = (fromIntegral t, [Firing ref a])


firingTrigger :: (MonadReflexHost t m, MonadIORef m) => Firing t -> m (Maybe (DSum (EventTrigger t)))
firingTrigger (Firing ref a) = fmap (:=> a) <$> readRef ref


runPlan :: (MonadReflexHost t m, MonadIORef m) => Plan t a -> m (a, Schedule t)
runPlan (Plan p) = runHostFrame $ runStateT p mempty


-- | Execute a plan, but add in extra frames to make it dense to properly test behaviors
-- range of samples is from 0, maxFrame + 1 (to catch any change resulting from the last event)
testPlan :: (MonadReflexHost t m, Readable t m a r, MonadIORef m) => Plan t a -> m (IntMap r)
testPlan  p = do
  (a, schedule) <- runPlan p
  readSchedule (makeDense schedule) =<< getRead a



makeDense :: Schedule t -> Schedule t
makeDense s = fromMaybe (emptyRange 0) $ do
  (end, _) <- fst <$> maxViewWithKey s
  return $ union s (emptyRange end)
    where
      emptyRange end = IntMap.fromList (zip [0..end + 1] (repeat []))



subscribePlan :: (MonadReflexHost t m, Readable t m a r, MonadIORef m) => a -> m (ReadPhase m r)
subscribePlan a = getRead a

execSchedule :: (MonadReflexHost t m, Readable t m a r, MonadIORef m) => (a, Schedule t) -> m (IntMap r)
execSchedule (a, schedule) = readSchedule schedule =<< getRead a

readSchedule :: (MonadReflexHost t m, MonadIORef m) => Schedule t -> ReadPhase m a -> m (IntMap a)
readSchedule schedule readResult = fmap IntMap.fromList $
  forM (IntMap.toList schedule) $ \(t, occs) -> do
    triggers <- catMaybes <$> traverse firingTrigger occs
    v <- fireEventsAndRead triggers readResult
    return (t, v)






