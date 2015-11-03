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

mergeTree :: (Monoid (f [Int]), Functor f) => Int -> [f Int] -> f Int
mergeTree n es | length es <= n =  sum' es
               | otherwise = mergeTree n subTrees
  where
    merges   = chunksOf n es
    subTrees = map sum' merges
    sum'     = fmap sum . mconcat . fmap (fmap (:[]) )

    

eventTree size branching = do
  (es, fires) <- unzip <$> for [1..size] (const newEventWithFire)
  return (mergeTree branching es, fires)

  
holdCounters size = do
  (es, fires) <- unzip <$> for [1..size] (const newEventWithFire)
  bs <- runHostFrame $ traverse (fmap snd . foldDyn (+) 0) es
  return (bs, fires)


pullTree size branching = do
  (bs, fires) <- holdCounters size
  
  let b = mergeTree branching bs
  
  for (transpose $ chunksOf 13 fires) $ \firing -> do
    fireEvents (zipWith ($) firing (repeat 1))
  
  x <- sampleIO b
  return (length fires == x)
  
  
  
mergeSubscribe size branching = do
  (e, fires) <- eventTree size branching
  subscribeEvent e
   
  
mergeFiring size branching = do
  (e, fires) <- eventTree size branching
  handle <- subscribeEvent e  
  
  for_ (transpose $ chunksOf 13 fires) $ \firing -> do
    fireEventsAndRead (zipWith ($) firing [1..]) (readEvent handle)

    
main = defaultMain 
  [ bench "pullTree" $ whnfIO $ pullTree size branching
  , bench "mergeSubscribe" $ whnfIO $ mergeSubscribe size branching
  , bench "mergeFiring" $ whnfIO $ mergeFiring size branching
  ]  
  
  where 
    size      = 10000
    branching = 8
