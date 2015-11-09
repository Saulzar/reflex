{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main (main) where

import Reflex
import Reflex.Host.Class

import Reflex.Dynamic
import Reflex.Test.Plan

import Reflex.Pure
import Reflex.Test.PurePlan

import Control.Applicative
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Traversable
import Data.Foldable
import System.Exit
import Data.Monoid

import Prelude

type TestE a = forall t m. TestPlan t m => m (Event t a)
type TestB a = forall t m. TestPlan t m => m (Behavior t a)

data TestCase  where
  TestE  :: (Show a, Eq a) => TestE a -> TestCase
  TestB  :: (Show a, Eq a) => TestB a -> TestCase

-- Helpers to declare test cases
testE :: (Eq a, Show a) => String -> TestE a -> (String, TestCase)
testE name test = (name, TestE test)

testB :: (Eq a, Show a) => String -> TestB a -> (String, TestCase)
testB name test = (name, TestB test)

readEvent' :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
readEvent' = readEvent >=> sequence

testAgreement :: TestCase -> IO Bool
testAgreement (TestE plan) = do
  r <- runSpiderHost $ do
    (e, s) <- runPlan plan
    h <- subscribeEvent e
    testSchedule s (readEvent' h)

  compareResult (r, testEvent $ runPure plan)
testAgreement (TestB plan) = do
  r <- runSpiderHost $ do
    (b, s) <- runPlan plan
    testSchedule s $ sample b

  compareResult (r, testBehavior $ runPure plan)


compareResult :: (Show a, Eq a) => (IntMap a, IntMap a) -> IO Bool
compareResult (r, r') = do
  when (r /= r') $ do
    putStrLn ("Got: " ++ show r)
    putStrLn ("Expected: " ++ show r')
  return (r == r')


main :: IO ()
main = do

   results <- forM testCases $ \(name, test) -> do
     putStrLn $ "Test: " <> name
     testAgreement test
   exitWith $ if and results
              then ExitSuccess
              else ExitFailure 1


testCases :: [(String, TestCase)]
testCases =
  [ testB "hold"  $ hold "0" =<< events1
  , testB "count" $ current <$> (count =<< events2)
  , testE "leftmost" $ liftA2 leftmost2 events1 events2
  , testE "onceE-1" $ do
      e <- events1
      onceE $ leftmost [e, e]

  , testE "switch-1" $ do
      e <- events1
      b <- hold never (e <$ e)
      return $ switch b

  , testE "switch-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
            switch <$> hold (leftmost ["a" <$ e, "b" <$ e]) (e <$ e)

  , testE "switch-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
          switch <$> hold (leftmost ["a" <$ e, "b" <$ e]) never

  , testE "switch-4" $ do
      e <- events1
      switch <$> hold (deep e) (e <$ e)

  , testE "switchPromptly-1" $ do
      e <- events1
      let e' = e <$ e
      switchPromptly never $ e <$ e

  , testE "switchPromptly-2" $ do
      e <- events1
      switchPromptly never $ deep (e <$ e)

  , testE "switchPromptly-3" $ do
      e <- events1
      switchPromptly never $ (e <$ deep e)

  , testE "switchPromptly-4" $ do
      e <- events1
      switchPromptly never $ (deep e <$ e)

  , testE "switch-5" $ do
      e <- events1
      switch <$> hold never (deep e <$ e)

  , testE "switchPromptly-5" $ do
    e <- events1
    switchPromptly never $ flip push e $
      const (Just <$> onceE e)

  , testE "switchPromptly-6" $ do
      e <- events1
      switchPromptly never $ flip pushAlways e $
        const (switchPromptly e never)

  , testE "coincidence-1" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (id <$> e)

  , testE "coincidence-2" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (deep e)

  , testE "coincidence-3" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const $ return (coincidence (e <$ e))

  , testE "coincidence-4" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $
        const (onceE e)

  , testE "coincidence-5" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = deep e
        return (coincidence (e' <$ e'))

  , testE "coincidence-6" $ do
      e <- events1
      return $ coincidence $ flip pushAlways e $ const $ do
        let e' = coincidence (e <$ e)
        return $ deep e'

  , testE "coincidence-7" $ do
      e <- events1
      return $ coincidence (deep e <$ e)

  , testB "holdWhileFiring" $ do
      e <- events1
      eo <- onceE e
      bb <- hold (constant "x") $ pushAlways (const $ hold "a" eo) eo
      return $ pull $ sample =<< sample bb

  , testE "joinDyn" $ do
      e <- events1
      bb <- hold "b" e
      bd <- hold never . fmap (const e) =<< onceE e

      eOuter <- liftM (pushAlways sample . fmap (const bb)) $ onceE e
      let eInner = switch bd
      return $ leftmost [eOuter, eInner]

  ] where

    events1, events2 :: forall t m. TestPlan t m => m (Event t String)
    events1 = plan [(1, "a"), (2, "b"), (3, "c"), (5, "d"), (8, "e")]
    events2 = plan [(1, "a"), (3, "b"), (4, "c"), (6, "d"), (8, "e")]

    deep e = leftmost [e, e]
    leftmost2 e1 e2 = leftmost [e1, e2]


