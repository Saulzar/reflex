{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Reflex.Test
  ( testAgreement
  , compareResult
  , runTests

  , module Reflex.TestPlan

  ) where

import Reflex
import Reflex.Host.Class

import Reflex.Ant

import Reflex.TestPlan

import Reflex.Pure
import Reflex.Plan.Pure
import Reflex.Plan.Reflex

import Control.Monad
import Data.Monoid

import Control.Applicative
import Data.IntMap (IntMap)

import Data.Traversable
import Data.Foldable
import System.Exit

import Prelude


testAgreement :: TestCase -> IO Bool
testAgreement (TestE plan) = do
  ant    <- runAntHost $ runTestE plan
  let results = [ ("ant", ant)]

  compareResult results (testEvent $ runPure plan)

testAgreement (TestB plan) = do
  ant    <- runAntHost $ runTestB plan
  let results = [ ("ant", ant)]

  compareResult results (testBehavior $ runPure plan)


compareResult :: (Show a, Eq a) => [(String, IntMap a)] -> IntMap a -> IO Bool
compareResult results expected = fmap and $ forM results $ \(name, r) -> do

  when (r /= expected) $ do
    putStrLn ("Got: " ++ show (name, r))
    putStrLn ("Expected: " ++ show expected)
  return (r == expected)


runTests :: [(String, TestCase)] -> IO ()
runTests testCases = do
   results <- forM testCases $ \(name, test) -> do
     putStrLn $ "Test: " <> name
     testAgreement test
   exitWith $ if and results
              then ExitSuccess
              else ExitFailure 1

