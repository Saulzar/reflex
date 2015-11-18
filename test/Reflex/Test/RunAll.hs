{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main (main) where

import Reflex.Test

import qualified Reflex.Bench.Focused as Focused
import qualified Reflex.Test.Micro as Micro
import Data.Bifunctor
import Data.List

import System.Exit
import System.Environment

import Prelude

matchPrefixes :: [String] -> (String -> Bool)
matchPrefixes []   = const True
matchPrefixes args = \name -> any (`isPrefixOf` name) args


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["--list"] -> mapM_ putStrLn (fst <$> allTests) >> exitWith (ExitFailure 1)
    _          -> case filter (matchPrefixes args . fst) allTests of
                    []    -> putStrLn "filter did not match any tests" >> exitWith (ExitFailure 1)
                    tests -> runTests tests

  where
    allTests = concat
     [ group "micro" Micro.testCases
     , group "subscribing (100,40)" (Focused.subscribing 100 40)
     , group "firing 1000" (Focused.firing 1000)
     ]

    group name tests = first (\test -> intercalate "/" [name, test]) <$> tests


