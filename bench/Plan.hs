{-# LANGUAGE ConstraintKinds, TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Data.List.Split
import Criterion.Main

import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal (SpiderEventHandle)
import Reflex.Dynamic

import Reflex.Ant
import qualified Reflex.Ant.Internal as Ant

import Reflex.Test.Plan
import Control.Monad.IO.Class

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.List
import Control.DeepSeq (NFData (..))
import Data.Bifunctor

import Data.IORef
import Control.Monad.Ref

import Prelude

type MonadReflexHost' t m = (MonadReflexHost t m, MonadIORef m, MonadIORef (HostFrame t))

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


instance NFData (Ant.EventHandle a) where
  rnf !_ = ()

instance NFData (SpiderEventHandle a) where
  rnf !_ = ()


setupFiring ::   (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (Ignore (EventHandle t a), Schedule t)
setupFiring plan = do
  (e, s) <- runPlan plan
  h <- subscribeEvent e
  return (Ignore h, s)

-- Hack to avoid the NFData constraint for EventHandle which is a synonym
newtype Ignore a = Ignore a
instance NFData (Ignore a) where
  rnf !_ = ()


-- Measure the running time
benchFiring ::  (MonadReflexHost' t m) => (forall a. m a -> IO a) -> (String, TestCase) -> Benchmark
benchFiring runHost (name, TestE plan) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (Ignore h, s) = runHost $ readSchedule s (readEvent' h)
    setup = runHost $ setupFiring plan

benchFiring runHost (name, TestB plan) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (b, s) = runHost $ readSchedule s (sample b)
    setup = runHost $ second makeDense <$> runPlan plan


benchSubscribe :: (MonadReflexHost' t m) => (forall a. m a -> IO a) -> (String, TestCase) -> Benchmark
benchSubscribe runHost (name, TestE plan) = bench name $ whnfIO $ runHost $
  fst <$> runPlan plan >>= subscribeEvent


benchAllFiring ::  (String, TestCase) -> Benchmark
benchAllFiring (name, test) = bgroup name
  [ benchFiring runSpiderHost ("spider", test)
  , benchFiring runAntHost ("ant", test)
  ]


benchAllSubscribe ::  (String, TestCase) -> Benchmark
benchAllSubscribe (name, test) = bgroup name
  [ benchSubscribe runSpiderHost ("spider", test)
  , benchSubscribe runAntHost ("ant", test)
  ]

main :: IO ()
main = defaultMain $ (benchmarks 1000 ++ benchmarks 10000)


benchmarks :: Word ->  [Benchmark]
benchmarks n =
  [ bgroup ("subscribe " ++ show n) $ benchAllSubscribe <$> subs
  , bgroup ("firing " ++ show n) $ benchAllFiring <$>  (subs ++ firing)
  ]
    where subs = subscribeBench n
          firing = firingBench n


subscribeBench :: Word -> [(String, TestCase)]
subscribeBench n =
  [ testE "fmapFan merge"       $ mergeList . fmapFan n <$> event
  , testE "fmapFan/mergeTree 8" $ mergeTree 8 . fmapFan n <$> event
  , testE "fmapChain"           $ fmapChain n <$> event
  , testE "switchChain"         $ switchChain n =<< event
  , testE "switchPromptlyChain" $ switchPromptlyChain n =<< event
  , testE "switchFactors"       $ switchFactors n =<< fmap (+1) <$> events 4
  , testE "coincidenceChain"    $ coinChain n <$> event
  ]


firingBench :: Word -> [(String, TestCase)]
firingBench n =
  [ testE "dense mergeTree 8"      $ mergeTree 8 <$> denseEvents n
  , testE "sparse 10/mergeTree 8"  $ mergeTree 8 <$> sparseEvents n 10
  , testE "runFrame"               $ events n
  , testB "sum counters" $ do
      counts <- counters n 10
      return $ pull $ sum <$> traverse sample counts

  , testB "pullChain"                 $ pullChain n . current <$> (count =<< events 4)
  , testB "mergeTree (pull) counters" $ mergeTree 8 <$> counters n 10
  ]


