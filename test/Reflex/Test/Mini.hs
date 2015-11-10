{-# LANGUAGE ScopedTypeVariables #-}

module Main where


-- import Test.QuickCheck hiding (sample)
-- import Test.QuickCheck.Monadic

import Reflex.Ant.Internal
import Data.Traversable
import Data.Foldable

import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid

runEventFrames ::  EventHandle a -> [[Trigger]] -> IO [Maybe a]
runEventFrames handle frames = traverse (flip fireEventsAndRead (readEvent handle)) frames

chainMerge :: Int -> Event [Int] -> Event [Int]
chainMerge depth e = iterate (fmap (fmap (+1)) . leftmost . pure) e !! depth

setupSwitching :: Int -> Int -> IO ([Event [Int]], [Trigger], [Event (Event [Int])], [Trigger], Int -> [Trigger])
setupSwitching depth size = do
  (inputs, fireInputs) <- unzip <$> for [1..size] (const newEventWithFire)
  (switchEvents, fireSwitches) <- unzip <$> for [1..size] (const newEventWithFire)
  (switched, fireSwitched) <- newEventWithFire

  let inputTriggers = fireSwitched [10] : zipWith ($) fireInputs (pure <$> [1..])
      resetTriggers = zipWith ($) fireSwitches inputs
      triggerSwitch i = [(fireSwitches !! i) (chainMerge depth switched)]

  return (inputs, inputTriggers, switchEvents, resetTriggers, triggerSwitch)



testCoincidence :: Int -> Int -> IO [[[Int]]]
testCoincidence depth size = do
  (inputs, inputTriggers, switchEvents, resetTriggers, triggerSwitch) <- setupSwitching depth size

  let events = zipWith (\e e' -> leftmost [coincidence e, e']) switchEvents inputs
      result = foldr1 mappend events

  handle <- subscribeEvent result
  let runInputsWith triggers = fireEventsAndRead (triggers <> inputTriggers) (readEvent handle)

      test1 i = triggerSwitch (i - 1)
      test2 i = triggerSwitch (size - i)
      test3 i = concatMap triggerSwitch [0..i-1]


  for [test1, test2, test3] $ \test -> do
    fmap catMaybes <$> for [1..size] $ \i -> runInputsWith (test i)


testSwitching :: Int -> Int -> IO [[[Int]]]
testSwitching depth size = do
  (inputs, inputTriggers, switchEvents, resetTriggers, triggerSwitch) <- setupSwitching depth size

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
    fmap catMaybes <$> for [1..size] $ \i -> test i >> runInputs


main = do
  for [0..4] $ \depth -> do
    testSwitching depth 4 >>= print

  putStrLn ""
  for [0..4] $ \depth -> do
    testCoincidence depth 4 >>= print


