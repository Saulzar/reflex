{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main
import Criterion.Types

import Control.Monad.Fix

import Reflex

import Reflex.Bench.Adjustable
import Test.Run

import Data.IntMap.Strict (IntMap)
import Data.These
import Data.Align (align)
import Data.Functor.Identity

import Data.Foldable


type Network a b = forall t m. (Adjustable t m, MonadHold t m, MonadFix m) => Event t a -> m (Event t b)

benchmarkApp ::  [Maybe a] -> Network a b -> Benchmarkable
benchmarkApp occs network = perRunEnv (runSetup network subscribeAndRead) (\f -> traverse_ f occs)




overhead :: Network a b -> [Maybe a] -> Benchmark
overhead app actions =  bgroup "overhead" $ 
  [ bench "base" $ benchmarkApp actions app  
  , bench "ReaderT" $ benchmarkApp actions (\input -> alignDyn <$> runDynamicWriterT (app input)) 

  , bench "DynamicWriterT" $ benchmarkApp actions (\input -> alignDyn <$> runDynamicWriterT (app input)) 
  , bench "EventWriterT" $ benchmarkApp actions (\input -> alignE <$> runEventWriterT (app input)) 
  , bench "RequesterT " $ benchmarkApp actions $ \input -> do
      rec
        (out, req) <- runRequesterT (app input) $ 
          traverseRequesterData Identity <$> req
      return (align out req)

  ] where

    alignDyn (e, d :: Dynamic t [b] ) = align e (updated d)
    alignE (e, e' :: Event t [b]) = align e e'





widget :: (Adjustable t m, MonadHold t m, MonadFix m) => Word -> Event t Word -> m (Event t Word)
widget a e = updated <$> foldDyn (+) a e


main :: IO ()
main = defaultMainWith config benchmarks where 
  config = defaultConfig 
    { timeLimit = 5
    }
  benchmarks = 
    [overhead (simpleApp mempty widget) (Just <$> addRemove 1000)
    ]
