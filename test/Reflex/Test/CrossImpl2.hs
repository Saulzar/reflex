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


testCases :: [(String, TestCase)]
testCases =
  [ testB "hold"  $ hold "0" =<< e1
  , testB "count" $ current <$> (count =<< e2)
  , testE "leftmost" $ liftA2 (\e e' -> leftmost [e, e']) e1 e2
  ] where

    e1, e2 :: TestPlan t m => m (Event t String)
    e1 = plan [(1, "a"), (2, "b"), (3, "c"), (5, "d"), (8, "e")]
    e2 = plan [(1, "a"), (3, "b"), (4, "c"), (6, "d"), (8, "e")]


testAgreement :: TestCase -> IO Bool
testAgreement (TestE plan) = do
  r <- runSpiderHost $ testPlan plan
  compareResult (r, testEvent $ runPure plan)
testAgreement (TestB plan) = do
  r <- runSpiderHost $ testPlan plan
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


{-
  , (,) "onceE-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       e' <- onceE $ leftmost [e, e]
       return (b, e')
  , (,) "switch-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       b' <- hold never e'
       let e'' = switch b'
       return (b, e'')
  , (,) "switch-2" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let ea = fmap (const "a") e
             let eb = fmap (const "b") e
             let eab = leftmost [ea, eb]
             liftM switch $ hold eab never
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-3" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let ea = fmap (const "a") e
             let eb = fmap (const "b") e
             let eab = leftmost [ea, eb]
             liftM switch $ hold eab (fmap (const e) e)
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-4" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- liftM switch $ hold e' (fmap (const e) e)
       return (b, e'')
  , (,) "switchPromptly-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never $ leftmost [e', e']
       return (b, e'')
  , (,) "switchPromptly-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e) e')
       return (b, e'')
  , (,) "switchPromptly-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e') e)
       return (b, e'')
  , (,) "switch-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- liftM switch $ hold never (fmap (const e') e)
       return (b, e'')
  , (,) "switchPromptly-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip push e $ \_ -> do
             return . Just =<< onceE e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> do
             switchPromptly e never
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "coincidence-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ fmap id e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ leftmost [e, e]
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ coincidence $ fmap (const e) e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> onceE e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let eNewValues = leftmost [e, e]
             return $ coincidence $ fmap (const eNewValues) eNewValues
           e' = coincidence eChild
       return (b, e')
  , (,) "coincidence-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let e' = coincidence $ fmap (const e) e
             return $ leftmost [e', e']
           e'' = coincidence eChild
       return (b, e'')
  , (,) "coincidence-7" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
           eCoincidences = coincidence $ fmap (const e') e
       return (b, eCoincidences)
  , (,) "holdWhileFiring" $ TestCase (Map.singleton 0 "zxc", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       eo <- onceE e
       bb <- hold b $ pushAlways (const $ hold "asdf" eo) eo
       let b' = pull $ sample =<< sample bb
       return (b', e)
  , (,) "joinDyn" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       bb <- hold "b" e
       bd <- hold never . fmap (const e) =<< onceE e
       eOuter <- liftM (pushAlways sample . fmap (const bb)) $ onceE e
       let eInner = switch bd
           e' = leftmost [eOuter, eInner]
       return (b, e')
  ]
-}

