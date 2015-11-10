{-# LANGUAGE TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Data.List.Split
import Criterion.Main

import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal (SpiderEventHandle)
import Reflex.Dynamic

import Reflex.Test.Plan
import Control.Monad.IO.Class

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.List
import Control.DeepSeq (NFData (..))
import Data.Bifunctor

import Prelude

mergeTree :: Num a => (Monoid (f [a]), Functor f) => Int -> [f a] -> f a
mergeTree n es | length es <= n =  sum' es
               | otherwise = mergeTree n subTrees
  where
    merges   = chunksOf n es
    subTrees = map sum' merges
    sum'     = fmap sum . mconcat . fmap (fmap (:[]) )

-- N events all firing in one frame
denseEvents :: TestPlan t m => Word -> m [Event t Word]
denseEvents n = for [1..n] $ \i -> plan [(1, i)]

-- Event which fires once on first frame
event :: TestPlan t m => m (Event t Word)
event = plan [(1, 0)]

-- Event which fires constantly over N frames
events :: TestPlan t m => Word -> m (Event t Word)
events n  = plan $ (\i -> (i, i)) <$> [1..n]

-- N events all originating from one event
fmapFan :: Reflex t => Word -> Event t Word -> [Event t Word]
fmapFan n e = (\i -> (+i) <$> e) <$> [1..n]

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
pullChain n b = iterateN (\b' -> pull $ sample b') b n

-- Give N events split across M frames approximately evenly
sparseEvents :: TestPlan t m => Word -> Word -> m [Event t Word]
sparseEvents n frames = do
  fmap concat $ for frameOccs $ \(frame, firing) ->
    for firing $ \i -> plan [(frame, i)]
  where
    frameOccs = zip [1..] $ transpose $ chunksOf (fromIntegral frames) [1..n]


counters :: TestPlan t m => Word -> Word -> m [Behavior t Int]
counters n frames = traverse (fmap current . count) =<< sparseEvents n frames



instance NFData (SpiderEventHandle a) where
  rnf !_ = ()

-- Measure the running time
benchFiring :: Bench  -> Benchmark
benchFiring (BenchE name plan) = env setup (\e -> bench name $ whnfIO $ run e)
  where
    run (h, s) = runSpiderHost $ readSchedule s (readEvent' h)
    setup = runSpiderHost $ do
      (e, s) <- runPlan plan
      h <- subscribeEvent e
      return (h, s)

benchFiring (BenchB name plan) = env setup (\e -> bench name $ whnfIO $ run e)
  where
    run (b, s) = runSpiderHost $ readSchedule s (sample b)
    setup = runSpiderHost $ second makeDense <$> runPlan plan


benchSubscribe :: Bench  -> Benchmark
benchSubscribe (BenchE name plan) = bench name $ whnfIO $ runSpiderHost $
  fst <$> runPlan plan >>= subscribeEvent


main :: IO ()
main = defaultMain $ (benchmarks 1000 ++ benchmarks 10000)


benchmarks :: Word ->  [Benchmark]
benchmarks n =
  [ bgroup ("subscribe " ++ show n) $ benchSubscribe <$> subs
  , bgroup ("firing " ++ show n) $ benchFiring <$> (subs ++ firing)
  ]
    where subs = subscribeBench n
          firing = firingBench n

data Bench where
  BenchE :: String -> TestE a -> Bench
  BenchB :: String -> TestB a -> Bench



subscribeBench :: Word -> [Bench]
subscribeBench n =
  [ BenchE "fmapFan merge"       $ mergeList . fmapFan n <$> event
  , BenchE "fmapFan/mergeTree 8" $ mergeTree 8 . fmapFan n <$> event
  , BenchE "fmapChain"           $ fmapChain n <$> event
  , BenchE "switchChain"         $ switchChain n =<< event
  , BenchE "switchPromptlyChain" $ switchPromptlyChain n =<< event
  , BenchE "switchFactors"       $ switchFactors n =<< fmap (+1) <$> events 4
  , BenchE "coincidenceChain"    $ coinChain n <$> event
  ]


firingBench :: Word -> [Bench]
firingBench n =
  [ BenchE "dense mergeTree 8"      $ mergeTree 8 <$> denseEvents n
  , BenchE "sparse 10/mergeTree 8"  $ mergeTree 8 <$> sparseEvents n 10
  , BenchE "runFrame"               $ events n
  , BenchB "sum counters" $ do
      counts <- counters n 10
      return $ pull $ sum <$> traverse sample counts

  , BenchB "pullChain"                 $ pullChain n . current <$> (count =<< events 4)
  , BenchB "mergeTree (pull) counters" $ mergeTree 8 <$> counters n 10
  ]


