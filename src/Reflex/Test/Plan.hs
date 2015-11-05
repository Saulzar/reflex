{-# LANGUAGE ExistentialQuantification, GADTs, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, BangPatterns, UndecidableInstances, EmptyDataDecls, RecursiveDo, RoleAnnotations, FunctionalDependencies, FlexibleContexts, StandaloneDeriving #-}
module Reflex.Test.Plan where

import Reflex.Class
import Reflex.Host.Class

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.State.Strict

import Data.Dependent.Sum (DSum (..))
import Data.Monoid
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.GADT.Compare
import Control.Monad.Ref

import Data.IntMap
import Data.IORef

-- Note: this import must come last to silence warnings from AMP
import Prelude

class (Reflex t, MonadHold t m) => TestPlan t m where
  -- | Speicify a plan of an input Event firing
  -- Occurances must be in the future (i.e. Time > 0)
  -- Initial specification is

  plan :: [(Int, a)] -> m (Event t a)

data Firing t where
  Firing :: IORef (Maybe (EventTrigger t a)) -> a -> Firing t


-- Implementation of a TestPlan
newtype Plan t a = Plan (StateT (IntMap [Firing t]) (HostFrame t) a)

deriving instance ReflexHost t => Functor (Plan t)
deriving instance ReflexHost t => Applicative (Plan t)
deriving instance ReflexHost t => Monad (Plan t)

deriving instance ReflexHost t => MonadSample t (Plan t)
deriving instance ReflexHost t => MonadHold t (Plan t)

instance (ReflexHost t, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => TestPlan t (Plan t) where
  plan occurances = Plan $ do
    (e, ref) <- newEventWithTriggerRef
    modify (IntMap.unionWith mappend (firings ref))
    return e

    where
      firings ref = IntMap.fromList (makeFiring ref <$> occurances)
      makeFiring ref (t, a) = (t, [Firing ref a])


readEvent' :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
readEvent' = readEvent >=> sequence

firingTrigger :: (MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO) => Firing t -> m (Maybe (DSum (EventTrigger t)))
firingTrigger (Firing ref a) = fmap (:=> a) <$> readRef ref


runPlan :: (MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO) => Plan t a -> (a -> m (ReadPhase m b)) -> m (IntMap b)
runPlan (Plan p) makeRead = do
  (a, schedule) <- runHostFrame $ runStateT p mempty
  read <- makeRead a

  vs <- forM (IntMap.toList schedule) $ \(t, occs) -> do
    triggers <- catMaybes <$> traverse firingTrigger occs
    v <- fireEventsAndRead triggers read
    return (t, v)

  return (IntMap.fromList vs)





