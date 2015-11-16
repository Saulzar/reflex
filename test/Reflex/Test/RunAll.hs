{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main (main) where

import Reflex.Test

import qualified Reflex.Bench.Focused as Focused
import qualified Reflex.Test.Micro as Micro

import Prelude


main :: IO ()
main = runTests $
  Micro.testCases ++
  Focused.subscribing 100 4 ++
  Focused.firing 1000



