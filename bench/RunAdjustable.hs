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
import Control.Monad.Reader

import Reflex

import Reflex.Bench.Adjustable
import Test.Run

import Data.IntMap.Strict (IntMap)
import Data.These
import Data.Align (align)
import Data.Functor.Identity

import Data.Foldable

type Widget t m = (Adjustable t m, MonadHold t m, MonadFix m)

type WidgetIO t m = (PerformEvent t m, MonadIO (Performable m)
                    , Adjustable t m, MonadHold t m, MonadFix m)


type Network a b = forall t m. Widget t m => Event t a -> m (Event t b)
type NetworkIO a b = forall t m. WidgetIO t m => Event t a -> m (Event t b)


benchmarkApp ::  [Maybe a] -> NetworkIO a b -> Benchmarkable
benchmarkApp occs network = perRunEnv (runSetup network subscribeAndRead) (\f -> traverse_ f occs)


runEcho :: Widget t m => RequesterT t Identity Identity m a -> m (a, Event t (RequesterData Identity)) 
runEcho app = do
  rec
    (out, req) <- runRequesterT app resp
    let resp = pushAlways (traverseRequesterData (return . response))  req

  return (out, req)
    where 
      response :: Identity a -> Identity a
      response = id

overhead :: NetworkIO a b -> [Maybe a] -> [Benchmark]
overhead app actions =  
  [ bench "base" $ benchmarkApp actions app 

  , bench "ReaderT" $ benchmarkApp actions $ 
      flip runReaderT () . app
  
  , bench "DynamicWriterT" $ benchmarkApp actions $ 
      fmap alignDyn . runDynamicWriterT . app 

  , bench "BehaviorWriterT" $ benchmarkApp actions $ 
      fmap attaching . runBehaviorWriterT . app       
  
  , bench "EventWriterT" $ benchmarkApp actions $ 
      fmap alignE . runEventWriterT . app

  , bench "RequesterT " $ benchmarkApp actions $  
      fmap (uncurry align) . runEcho . app 
      
  ] where

    alignDyn (e, d :: Dynamic t [b] ) = align e (updated d)
    alignE (e, e' :: Event t [b]) = align e e'

    attaching (e, b :: Behavior t [b]) = b `attach` e




widget :: Widget t m => Word -> Event t Word -> m (Event t Word)
widget a e = updated <$> foldDyn (+) a e

widgetIO :: WidgetIO t m => Word -> Event t Word -> m (Event t Word)
widgetIO a e = do 
  e' <- performEvent (return . (+1) <$> e)
  updated <$> foldDyn (+) a e'

widgetIOAdjusting :: WidgetIO t m => Word -> Event t Word -> m (Event t Word)
widgetIOAdjusting a e = do
  (e0, e') <- runWithReplace (widgetIO a e) (flip widgetIO e <$> update)
  switchHold e0 e'

  where
    update = ffilter (\n -> n `mod` 4 == 0) e 


main :: IO ()
main = defaultMainWith config benchmarks where 
  config = defaultConfig 
    { timeLimit = 1
    }
  benchmarks = 
    [ bgroup "overhead" $ 
        overhead (simpleApp mempty widget) actions1000

    , bgroup "overhead io" $ 
        overhead (simpleApp mempty widgetIO) actions1000

    , bgroup "overhead io adjusting" $ 
        overhead (simpleApp mempty widgetIOAdjusting) actions1000

    ]

    where 
      actions1000 = Just <$> addRemove 200 8

