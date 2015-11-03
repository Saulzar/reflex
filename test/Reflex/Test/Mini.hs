{-# LANGUAGE ScopedTypeVariables #-}

module Main where


-- import Test.QuickCheck hiding (sample)
-- import Test.QuickCheck.Monadic

import Reflex.MiniSpider
import Data.Traversable
import Data.Foldable

import Control.Monad
import Data.List

runEventFrames ::  EventHandle a -> [[Trigger]] -> IO [Maybe a]
runEventFrames handle frames = traverse (flip fireEventsAndRead (readEvent handle)) frames

chainMerge :: Int -> Event [Int] -> Event [Int]
chainMerge depth e = iterate (fmap (fmap (+1)) . leftmost . pure) e !! depth

testSwitching :: Int -> Int -> IO [[Maybe [Int]]]
testSwitching depth size = do
  (inputs, fireInputs) <- unzip <$> for [1..size] (const newEventWithFire)
  (switchEvents, fireSwitches) <- unzip <$> for [1..size] (const newEventWithFire)
  (switched, fireSwitched) <- newEventWithFire


  let inputTriggers = fireSwitched [10] : zipWith ($) fireInputs (pure <$> [1..])
      resetTriggers = zipWith ($) fireSwitches inputs
      triggerSwitch i = [(fireSwitches !! i) (chainMerge depth switched)]


  holds <- runHostFrame $ zipWithM hold inputs switchEvents
  let result = foldr1 mappend (switch <$> holds)

  handle <- subscribeEvent result
  let runInputs = fireEventsAndRead inputTriggers (readEvent handle)
      test1 i = fireEvents (triggerSwitch (i - 1))
      test2 i = fireEvents (triggerSwitch (size - i))

      test3 i = do
        fireEvents resetTriggers
        fireEvents (triggerSwitch (i - 1))

  for [test1, test2, test3] $ \test -> do
    fireEvents resetTriggers
    for [1..size] $ \i -> test i >> runInputs


main = for [1..5] $ \depth ->
  testSwitching depth 5 >>= print




