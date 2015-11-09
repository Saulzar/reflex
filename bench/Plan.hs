{-# LANGUAGE BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Data.List.Split
import Criterion.Main

import Reflex
import Reflex.Host.Class
import Reflex.Dynamic

import Reflex.Test.Plan
import Control.Monad.IO.Class

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.List

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
frames n  = plan $ (\i -> (n, n)) <$> [1..n]

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


-- Measure the running time
benchFiring :: String -> Plan Spider (Event Spider a)  -> Benchmark
benchFiring name plan = env (runSpiderHost $ runPlan plan)
  (\s -> bench name $ whnfIO $ runSpiderHost $ execSchedule s)


benchSubscribe :: String -> Plan Spider (Event Spider a)  -> Benchmark
benchSubscribe name plan = bench name (whnfIO $ runSpiderHost $ fst <$> runPlan plan >>= subscribeEvent)


main = defaultMain
  [ bgroup "subscribe"
    [ benchSubscribe "fmapFan/merge" $ mergeList . fmapFan 10000 <$> one
    , benchSubscribe "fmapFan/mergeTree 8" $ mergeTree 8 . fmapFan 10000 <$> one
    , benchSubscribe "fmapChain" $ fmapChain 10000 <$> one
    ]


  , bgroup "merges"
    [ benchFiring "dense mergeTree 8" $ mergeTree 8 <$> denseEvents 10000
    , benchFiring "sparse 10/mergeTree 8" $ mergeTree 8 <$> sparseEvents 10000 10
    , benchFiring "frames" $ frames 10000
    ]

  ]


