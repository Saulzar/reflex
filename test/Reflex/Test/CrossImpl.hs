{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main (main) where

import Reflex
import Reflex.Host.Class

import Reflex.Ant

import Reflex.Dynamic
import Reflex.Test.Plan

import Reflex.Pure
import Reflex.Test.PurePlan

import Control.Applicative
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Char
import Data.Traversable
import Data.Foldable
import System.Exit
import Data.Monoid

import Prelude



testAgreement :: TestCase -> IO Bool
testAgreement (TestE plan) = do
  spider <- runSpiderHost $ runTestE plan
  ant    <- runAntHost $ runTestE plan
  let results = [("spider", spider), ("ant", ant)]

  compareResult results (testEvent $ runPure plan)

testAgreement (TestB plan) = do
  spider <- runSpiderHost $ runTestB plan
  ant    <- runAntHost $ runTestB plan
  let results = [("spider", spider), ("ant", ant)]

  compareResult results (testBehavior $ runPure plan)


compareResult :: (Show a, Eq a) => [(String, IntMap a)] -> IntMap a -> IO Bool
compareResult results expected = fmap and $ forM results $ \(name, r) -> do

  when (r /= expected) $ do
    putStrLn ("Got: " ++ show (name, r))
    putStrLn ("Expected: " ++ show expected)
  return (r == expected)


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
  , testB "pull"  $ do
      b <- hold "0" =<< events1
      return (id <$> id <$> b)


  , testB "pull-2" $ do
      b1 <- behavior1
      return (id <$> pull $ liftA2 (<>) (sample b1) (sample b1))

  , testB "pull-3" $ do
      b1 <- behavior1
      b2 <- behavior2
      return (id <$> pull $ liftA2 (<>) (sample b1) (sample b2))

  , testE "tag-1" $ do
      b1 <- behavior1
      e <- events2
      return (tag b1 e)

  , testE "tag-2" $ do
      b1 <- behavior1
      e <- events2
      return (tag (map toUpper <$>  b1) e)

  , testE "attach-1" $ do
      b1 <- behavior1
      e <- events2
      return (attachWith (++) (map toUpper <$> b1) e)

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

  , testB "foldDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      return (current d)

  , testB "mapDyn"  $ do
      d <- foldDyn (++) "0" =<< events1
      current <$> mapDyn (map toUpper) d

  , testB "combineDyn"  $ do
      d1 <- foldDyn (++) "0" =<< events1
      d2 <- mapDyn (map toUpper) =<< foldDyn (++) "0" =<< events2

      current <$> combineDyn (<>) d1 d2

  ] where

    events1, events2 :: forall t m. TestPlan t m => m (Event t String)
    events1 = plan [(1, "a"), (2, "b"), (3, "c"), (5, "d"), (8, "e")]
    events2 = plan [(1, "a"), (3, "b"), (4, "c"), (6, "d"), (8, "e")]

    behavior1, behavior2 :: forall t m. TestPlan t m => m (Behavior t String)
    behavior1 =  hold "1" =<< events1
    behavior2 =  hold "2" =<< events2


    deep e = leftmost [e, e]
    leftmost2 e1 e2 = leftmost [e1, e2]


