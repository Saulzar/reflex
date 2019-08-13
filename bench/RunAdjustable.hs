{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main
import Criterion.Types

import Reflex
import Reflex.Host.Class

import Reflex.Test.Adjustable

import Data.Map (Map)
import qualified Data.Map as Map



benchmarks :: [(String, Int -> IO ())]
benchmarks = implGroup "spider" runSpiderHost cases
  where
    implGroup :: (MonadReflexHost' t m, MonadSample t m) => String -> (forall a. m a -> IO a) -> [(String, TestCase)] -> [(String, Int -> IO ())]
    implGroup name runHost = group name . fmap (second (benchFiring runHost))
    group name = fmap $ first ((name <> "/") <>)
    sub n frames = group ("subscribing " ++ show (n, frames)) $ Focused.subscribing n frames
    firing n     = group ("firing "    <> show n) $ Focused.firing n
    merging n    = group ("merging "   <> show n) $ Focused.merging n
    dynamics n   = group ("dynamics "  <> show n) $ Focused.dynamics n
    cases = concat
      [ sub 100 40
      , dynamics 100
      , dynamics 1000
      , firing 1000
      , firing 10000
      , merging 10
      , merging 50
      , merging 100
      , merging 200
      ]

pattern RunTestCaseFlag = "--run-test-case"

spawnBenchmark :: String -> Benchmark
spawnBenchmark name = bench name . toBenchmarkable $ \n -> do
  self <- getExecutablePath
  callProcess self [RunTestCaseFlag, name, show n, "+RTS", "-N1"]

foreign import ccall unsafe "myCapabilityHasOtherRunnableThreads" myCapabilityHasOtherRunnableThreads :: IO Bool

main :: IO ()
main = defaultMainWith $ fmap (spawnBenchmark . fst) benchmarks
  where config = defaultConfig { timeLimit = 20, csvFile = Just "dmap-original.csv", reportFile = Just "report.html" }
