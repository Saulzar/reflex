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
one :: TestPlan t m => m (Event t Word)
one = plan [(1, 0)]

-- Event which fires constantly over N frames
frames :: TestPlan t m => Word -> m (Event t Word)
frames n  = plan $ (\i -> (i, i)) <$> [1..n]

-- N events all originating from one event
fmapFan :: Reflex t => Word -> Event t Word -> [Event t Word]
fmapFan n e = (\i -> (+i) <$> e) <$> [1..n]

fmapChain :: Reflex t => Word -> Event t Word -> (Event t Word)
fmapChain n e = (iterate (fmap (+1)) e) !! fromIntegral n


-- Give N events split across M frames approximately evenly
sparseEvents :: TestPlan t m => Word -> Word -> m [Event t Word]
sparseEvents n frames = do
  fmap concat $ for frameOccs $ \(frame, firing) ->
    for firing $ \i -> plan [(frame, i)]
  where
    frameOccs = zip [1..] $ transpose $ chunksOf (fromIntegral frames) [1..n]


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

benchSubscribe :: Bench  -> Benchmark
benchSubscribe (BenchE name plan) = bench name $ whnfIO $ runSpiderHost $
  fst <$> runPlan plan >>= subscribeEvent


main :: IO ()
main = defaultMain $  benchmarks 10000


benchmarks :: Word ->  [Benchmark]
benchmarks n =
  [ bgroup ("subscribe " ++ show n) $ benchSubscribe <$> subs
  , bgroup ("firing " ++ show n) $ benchFiring <$> (subs ++ firing)
  ]
    where subs = subscribeBench n
          firing = firingBench n

data Bench where
  BenchE :: String -> (forall t m. TestPlan t m => m (Event t a)) -> Bench


subscribeBench :: Word -> [Bench]
subscribeBench n =
  [ BenchE "fmapFan/merge" $ mergeList . fmapFan n <$> one
  , BenchE "fmapFan/mergeTree 8" $ mergeTree 8 . fmapFan n <$> one
  , BenchE "fmapChain" $ fmapChain n <$> one
  ]

firingBench :: Word -> [Bench]
firingBench n =
  [ BenchE "dense mergeTree 8" $ mergeTree 8 <$> denseEvents 10000
  , BenchE "sparse 10/mergeTree 8" $ mergeTree 8 <$> sparseEvents 10000 10
  , BenchE "frames" $ frames 10000
  ]


