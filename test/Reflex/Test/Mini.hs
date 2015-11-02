module Main where

import Criterion.Main
import Data.List.Split

-- import Test.QuickCheck hiding (sample)
-- import Test.QuickCheck.Monadic

import Reflex.MiniSpider
import Data.Traversable
import Data.Foldable

import Control.Monad
import Data.List

runEventFrames :: [[Trigger]] -> EventHandle a -> IO [a]
runEventFrames frames handle = traverse (flip fireEventsAndRead (readEvent handle)) frames



testSwitching size = do

  (switchEvents, fireSwitches) <- unzip <$> for [1..size] (const newEventWithFire)

  holds <- runHostFrame $ sequence $ zipWith hold inputs switchEvents
  result <- foldr1 mappend (switch <$> holds)

  (inputs, fireInputs) <- unzip <$> for [1..size] (const newEventWithFire)
  let fireInputs = zipWith ($) fireInputs [1..]
      fireResets = zipWith ($) fireSwitches inputs







