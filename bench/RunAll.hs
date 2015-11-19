{-# LANGUAGE ConstraintKinds, TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Criterion.Main

import Reflex
import Reflex.Host.Class
import Reflex.Dynamic

import Reflex.Ant
import Reflex.TestPlan
import Reflex.Plan.Reflex

import qualified Reflex.Ant.Internal as Ant
import Reflex.Spider.Internal (SpiderEventHandle)
import qualified Reflex.Bench.Focused as Focused

import Control.Monad.IO.Class

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.DeepSeq (NFData (..))
import Data.Bifunctor

import Data.IORef
import Control.Monad.Ref

import System.Mem
import Prelude

type MonadReflexHost' t m = (MonadReflexHost t m, MonadIORef m, MonadIORef (HostFrame t))


setupFiring ::   (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (Ignore (EventHandle t a), Schedule t)
setupFiring plan = do
  (e, s) <- runPlan plan
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
  rnf !(Firing tr a) = ()

-- Measure the running time
benchFiring ::  (MonadReflexHost' t m, MonadSample t m) => (forall a. m a -> IO a) -> (String, TestCase) -> Benchmark
benchFiring runHost (name, TestE plan) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (Ignore h, s) = runHost (readSchedule s (readEvent' h)) >> performGC
    setup = runHost $ setupFiring plan

benchFiring runHost (name, TestB plan) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (b, s) = runHost (readSchedule s (sample b)) >> performGC
    setup = runHost $ do
      (b, s) <- runPlan plan
      return (b, makeDense s)


-- main = runAntHost $ do
--    (Ignore h, s) <- setupFiring $ Focused.switches 100 (Focused.switchFactors 100)
--    readSchedule s (readEvent' h)



benchAll ::  (String, TestCase) -> Benchmark
benchAll (name, test) = bgroup name
  [ benchFiring runSpiderHost ("spider", test)
  , benchFiring runAntHost ("ant", test)
  ]


main :: IO ()
main = defaultMain  [sub 100 40, firing 1000,  firing 10000, merging 50, merging 100]
  where
    sub n frames = bgroup ("subscribing " ++ show (n, frames)) $ benchAll <$> Focused.subscribing n frames
    firing n =  bgroup ("firing " ++ show n) $ benchAll <$>  Focused.firing n
    merging n = bgroup ("merging " ++ show n) $ benchAll <$> Focused.merging n



