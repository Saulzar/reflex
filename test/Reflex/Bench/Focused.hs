{-# LANGUAGE ConstraintKinds, TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Reflex.Bench.Focused where

import Reflex
import Reflex.TestPlan

import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import Data.Foldable
import Data.Traversable

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Data.List
import Data.List.Split

import Debug.Trace

import Prelude


mergeTree :: Num a => (Monoid (f [a]), Functor f) => Int -> [f a] -> f a
mergeTree n es | length es <= n =  sum' es
               | otherwise = mergeTree n subTrees
  where
    merges   = chunksOf n es
    subTrees = map sum' merges
    sum'     = fmap sum . mconcat . fmap (fmap (:[]) )

-- N events all firing for N frames
continuous :: TestPlan t m => Word -> Word -> m [Event t Word]
continuous n frames = for [1..n] $ \i -> plan $ (, i) <$> [1..frames]

-- Fire N events once every P frames for total N frames (interspersed evenly)
occasional :: TestPlan t m => Word -> Word -> Word -> m [Event t Word]
occasional n period frames = traverse plan $ zipWith occs [1..] phases
  where
    phases       = take (fromIntegral n) $ concat (repeat [1..period])
    occs i phase = (, i) <$> [1, phase+1..frames]



-- Event which never fires (but isn't 'never')
never' :: TestPlan t m => m (Event t Word)
never' = plan []


-- Event which fires once on first frame
event :: TestPlan t m => m (Event t Word)
event = plan [(1, 0)]

-- Event which fires constantly over N frames
events :: TestPlan t m => Word -> m (Event t Word)
events n  = plan $ (\i -> (i, i)) <$> [1..n]


-- N events all originating from one event
fmapFan :: Reflex t => Word -> Event t Word -> [Event t Word]
fmapFan n e = (\i -> (+i) <$> e) <$> [1..n]

-- Create an Event with a Map which is dense to size N
denseMap :: TestPlan t m => Word -> m (Event t (Map Word Word))
denseMap n = plan [(1, m)] where
  m = Map.fromList $ zip [1..n] [1..n]

-- Create an Event with a Map which is sparse, firing N events over M frames and Fan size S
-- sparseMap ::
-- sparseMap n frames size =   events frames
--   occs = transpose $ chunksOf frames $ flip zip [1..n * frames] $ take (n * frames) $ concat (replicate [1..size])


iterateN :: (a -> a) -> a -> Word -> a
iterateN f a n = iterate f a !! fromIntegral n

fmapChain :: Reflex t => Word -> Event t Word -> (Event t Word)
fmapChain n e = iterateN (fmap (+1)) e n


switchFactors :: (Reflex t, MonadHold t m) => Word -> Event t Word -> m (Event t Word)
switchFactors n e = iter n e where
  iter 0 e = return e
  iter n e = do
    b <- hold ((+1) <$> e) (e <$ ffilter ((== 0) . (n `mod`)) e)
    iter (n - 1) (switch b)


switchChain :: (Reflex t, MonadHold t m) => Word -> Event t Word -> m (Event t Word)
switchChain n e = iter n e where
  iter 0 e = return e
  iter n e = do
    b <- hold e (e <$ e)
    iter (n - 1) (switch b)

switchPromptlyChain :: (Reflex t, MonadHold t m) => Word -> Event t Word -> m (Event t Word)
switchPromptlyChain n e = iter n e where
  iter 0 e = return e
  iter n e = do
    d <- holdDyn e (e <$ e)
    iter (n - 1) (switchPromptlyDyn d)

coinChain :: Reflex t => Word -> Event t Word -> Event t Word
coinChain n e = iterateN (\e' -> coincidence (e' <$ e')) e n


pullChain :: Reflex t => Word -> Behavior t Word -> Behavior t Word
pullChain n b = iterateN (fmap (+1)) b n




-- | Data type for a collection (a map) which is updated by providing differences
data UpdatedMap t k a = UpdatedMap (Map k a) (Event t (Map k (Maybe a)))

-- This will hopefully become a primitive (faster!)
switchMergeEvents ::  (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a)  -> m (Event t (Map k a))
switchMergeEvents mapChanges = switch . fmap mergeMap  <$> holdMap mapChanges


switchMerge' :: (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Event t a)  -> m (Event t (Map k a))
switchMerge' (UpdatedMap initial changes) = switchMergeMap initial (fmap (fromMaybe never) <$> changes)


switchMergeBehaviors  :: forall a t m k. (MonadFix m, MonadHold t m, Reflex t, Ord k) =>  UpdatedMap t k (Behavior t a)  -> m (Behavior t (Map k a))
switchMergeBehaviors mapChanges = pull <$> joinMap <$> holdMap mapChanges
  where joinMap m = traverse sample =<< sample m


-- | Turn an UpdatedMap into a Dynamic by applying the differences to the initial value
holdMapDyn :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => UpdatedMap t k a -> m (Dynamic t (Map k a))
holdMapDyn (UpdatedMap initial changes) = foldDyn (flip (Map.foldWithKey modify)) initial changes

  where
    modify k Nothing items = Map.delete k items
    modify k (Just item) items = Map.insert k item items


-- | Hold an UpdatedMap as a behavior by applying differences to the initial value
holdMap :: (Reflex t, MonadHold t m, MonadFix m, Ord k) => UpdatedMap t k a -> m (Behavior t (Map k a))
holdMap = (current <$>) . holdMapDyn


increasingMerge :: TestPlan t m => [a] -> m (UpdatedMap t Int a)
increasingMerge xs = do
  e   <- planList (zipWith Map.singleton  [1..] (Just <$> xs) ++ [reset])
  return (UpdatedMap Map.empty e)
    where
      reset = Map.fromList $ zip [1..] (const Nothing <$> xs)

decreasingMerge ::  (TestPlan t m) => [a] -> m (UpdatedMap t Int a)
decreasingMerge xs = do
  e <- planList (zipWith Map.singleton  [1..] (const Nothing <$> xs) ++ [reset])
  return (UpdatedMap initial e)
    where
      initial = Map.fromList $ zip [1..] xs
      reset = Just <$> initial

modifyMerge :: (TestPlan t m) =>  [a] -> m (UpdatedMap t Int a)
modifyMerge xs = do
  e <- planList $ zipWith Map.singleton  [1..] (Just <$> xs)
  return (UpdatedMap initial e)
    where
      initial = Map.fromList $ zip [1..] xs


countMany :: (Reflex t, MonadHold t m, MonadFix m) => [Event t a] -> m [Behavior t Int]
countMany = traverse (fmap current . count)


countManyDyn :: (Reflex t, MonadHold t m, MonadFix m) => [Event t a] -> m [Dynamic t Int]
countManyDyn = traverse count


-- Given a function which creates a sub-network (from a single event)
-- Create a plan which uses a switch to substitute it
switches :: TestPlan t m => Word -> (forall m. MonadHold t m => Event t Word -> m (Event t a)) -> m (Event t a)
switches numFrames f = do
  es <- events numFrames
  switch <$> hold never (makeEvents es)

    where
      makeEvents es =  pushAlways (\n -> f (fmap (+n) es)) es

-- Two sets of benchmarks, one which we're interested in testing subscribe time (as well as time to fire frames)
-- the other, which we're only interested in time for running frames
subscribing :: Word -> Word -> [(String, TestCase)]
subscribing n frames =
  [ testE "fmapFan merge"       $ switches frames (return . mergeList . fmapFan n)
  , testE "fmapFan/mergeTree" $ switches frames (return . mergeTree 8 . fmapFan n)
  , testE "fmapChain"           $ switches frames (return . fmapChain n)
  , testE "switchChain"         $ switches frames (switchChain n)
  , testE "switchPromptlyChain" $ switches frames (switchPromptlyChain n)
  , testE "switchFactors"       $ switches frames (switchFactors n)
  , testE "coincidenceChain"    $ switches frames (return . coinChain n)
  ]


-- Set of benchmark to test specifically merging dynamic collections
-- a pattern which occurs frequently in reflex-dom collections
merging :: Word -> [(String, TestCase)]
merging n =
  [ testE "static events"        $ mergeList <$> sparse
  , testE "increasing events"    $ switchMergeEvents =<< increasingMerge =<< sparse
  , testE "decreasing events"    $ switchMergeEvents =<< decreasingMerge =<< sparse
  , testE "modify events"        $ switchMergeEvents =<< modifyMerge =<< sparse

  , testB "static behaviors"     $ pull . traverse sample <$> counters
  , testB "increasing behaviors" $ switchMergeBehaviors =<< increasingMerge =<< counters
  , testB "decreasing behaviors" $ switchMergeBehaviors =<< decreasingMerge =<< counters
  , testB "modify behaviors"     $ switchMergeBehaviors =<< modifyMerge =<< counters


  , testE "increasing events/switchMerge"    $ switchMerge' =<< increasingMerge =<< sparse
  , testE "decreasing events/switchMerge"    $ switchMerge' =<< decreasingMerge =<< sparse
  , testE "modify events/switchMerge"        $ switchMerge' =<< modifyMerge =<< sparse
  ]

  where
    sparse :: TestPlan t m => m [Event t Word]
    sparse = fmap (fmap (+1)) <$> occasional n 8 n

    counters :: TestPlan t m => m [Behavior t Int]
    counters = countMany =<< sparse


firing :: Word -> [(String, TestCase)]
firing n =
  [ testE "dense mergeTree"      $ mergeTree 8 <$> dense
  , testE "sparse mergeTree"  $ mergeTree 8 <$> sparse
  , testE "runFrame"               $ events n
  , testB "sum counters" $ do
      counts <- counters
      return $ pull $ sum <$> traverse sample counts

  , testB "pullChain"                 $ pullChain n . current <$> (count =<< events 4)
  , testB "pullChain2"                $ do
    e <- events 4
    b <- hold (constant 0) $ leftmost
      [ constant 0 <$ ffilter (==4) e
      , pushAlways (const $ pullChain n <$> hold 0 e) e
      ]
    return (join b)
  , testB "counters/pullTree" $ mergeTree 8 <$> counters

  ]
    where
      dense, sparse :: TestPlan t m => m [Event t Word]
      dense = continuous n 2
      sparse = occasional n 8 16

      counters :: TestPlan t m => m [Behavior t Int]
      counters = countMany =<< sparse



