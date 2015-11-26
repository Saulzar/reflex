{-# LANGUAGE ConstraintKinds, TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main

import Reflex
import Reflex.Host.Class

import Reflex.Ant
import Reflex.TestPlan
import Reflex.Plan.Reflex

import qualified Reflex.Ant.Internal as Ant
import Reflex.Spider.Internal (SpiderEventHandle)
import qualified Reflex.Bench.Focused as Focused

import Control.Applicative
import Control.DeepSeq (NFData (..))

import System.Mem
import Prelude

type MonadReflexHost' t m = (MonadReflexHost t m, MonadIORef m, MonadIORef (HostFrame t))


setupFiring ::   (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (Ignore (EventHandle t a), Schedule t)
setupFiring p = do
  (e, s) <- runPlan p
  h <- subscribeEvent e
  return (Ignore h, s)

-- Hack to avoid the NFData constraint for EventHandle which is a synonym
newtype Ignore a = Ignore a
instance NFData (Ignore a) where
  rnf !_ = ()

instance NFData (Ant.EventHandle a) where
  rnf !_ = ()

instance NFData (SpiderEventHandle a) where
  rnf !_ = ()

instance NFData (Behavior t a) where
  rnf !_ = ()

instance NFData (Firing t) where
  rnf !(Firing _ _) = ()

-- Measure the running time
benchFiring ::  (MonadReflexHost' t m, MonadSample t m) => (forall a. m a -> IO a) -> (String, TestCase) -> Benchmark
benchFiring runHost (name, TestE p) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (Ignore h, s) = runHost (readSchedule s (readEvent' h)) >> performGC
    setup = runHost $ setupFiring p

benchFiring runHost (name, TestB p) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (b, s) = runHost (readSchedule s (sample b)) >> performGC
    setup = runHost $ do
      (b, s) <- runPlan p
      return (b, makeDense s)



benchAll ::  (String, TestCase) -> Benchmark
benchAll (name, test) = bgroup name
  [ benchFiring runSpiderHost ("spider", test)
  , benchFiring runAntHost ("ant", test)
  ]


main :: IO ()
main = defaultMain  [sub 100 40, dynamics 100, dynamics 1000, firing 1000,  firing 10000, merging 10, merging 50, merging 100, merging 200]
  where
    sub n frames = bgroup ("subscribing " ++ show (n, frames)) $ benchAll <$> Focused.subscribing n frames
    firing n =  bgroup ("firing " ++ show n) $ benchAll <$>  Focused.firing n
    merging n = bgroup ("merging " ++ show n) $ benchAll <$> Focused.merging n
    dynamics n = bgroup ("dynamics " ++ show n) $ benchAll <$> Focused.dynamics n




