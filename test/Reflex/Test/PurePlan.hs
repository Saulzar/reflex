{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Reflex.Test.PurePlan where

import Reflex
import Reflex.Pure
import Reflex.Test.Plan

import Control.Monad.Fix
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Traversable
import Data.Foldable
import Data.Monoid
import Data.Bifunctor
import Data.Maybe
import Prelude


mapToPureEvent :: IntMap a -> Event (Pure Int) a
mapToPureEvent m = Event $ flip IntMap.lookup m

type TimeM = (->) Int
newtype PurePlan a = PurePlan { unPlan :: StateT IntSet TimeM a } deriving (Functor, Applicative, Monad, MonadFix)

liftPlan :: TimeM a -> PurePlan a
liftPlan = PurePlan . lift

instance MonadHold (Pure Int) PurePlan where
  hold initial  = liftPlan . hold initial

instance MonadSample (Pure Int) PurePlan where
  sample = liftPlan . sample


instance TestPlan (Pure Int) PurePlan where
  plan occs = do
    PurePlan . modify $ IntSet.union (IntMap.keysSet m)
    return $ mapToPureEvent m
      where m = IntMap.fromList (first fromIntegral <$> occs)

runPure :: PurePlan a -> (a, IntSet)
runPure (PurePlan p) = runStateT p mempty $ 0

relavantTimes :: IntSet -> IntSet
relavantTimes occs = IntSet.fromList [0..last + 1]
  where last = fromMaybe 0 (fst <$> IntSet.maxView occs)

testBehavior :: (Behavior (Pure Int) a, IntSet) -> IntMap a
testBehavior (b, occs) = IntMap.fromSet (sample b) (relavantTimes occs)

testEvent :: (Event (Pure Int) a, IntSet) -> IntMap (Maybe a)
testEvent (Event readEvent, occs) = IntMap.fromSet readEvent (relavantTimes occs)






