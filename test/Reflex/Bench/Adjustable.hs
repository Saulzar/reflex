{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}


module Reflex.Bench.Adjustable where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Reflex


data Action
  = Change  (IntMap (Maybe Word))
  | Event   (IntMap Word)
  deriving (Show)


maybeChange :: Action -> Maybe (IntMap (Maybe Word))
maybeChange (Change m) = Just m
maybeChange _ = Nothing

maybeEvent :: Action -> Maybe (IntMap Word)
maybeEvent (Event m) = Just m
maybeEvent _ = Nothing


traverseIntMapView ::  (Adjustable t m, MonadHold t m) 
                => IntMap v -> Event t (IntMap (Maybe v)) 
                -> (Int -> v -> m (Event t a)) 
                -> m (Event t (IntMap a))
traverseIntMapView m0 m' f = do
  (e0, e') <- traverseIntMapWithKeyWithAdjust f m0 (PatchIntMap <$> m')
  mergeIntMapIncremental <$> holdIncremental e0 e'


add :: Int -> Word -> Action
add k a =  Change (IntMap.singleton k (Just a))

delete :: Int -> Action
delete k  = Change (IntMap.singleton k Nothing)

event :: Int -> Word -> Action
event k a = Event (IntMap.singleton k a)



simpleApp :: (Reflex t, MonadHold t m, Adjustable t m) 
          => IntMap Word -> (Word -> Event t Word -> m (Event t a)) -> Event t Action -> m (Event t (IntMap a))
simpleApp initial widget action = traverseIntMapView initial (maybeChange `mapMaybe` action) widget' where
    
    events = fanInt (maybeEvent `mapMaybe` action)
    widget' k v = widget v (selectInt events k) 
      

addRemove :: Int -> [Action]
addRemove n = mconcat . fmap mconcat $ [adding <$> [1..n], firing <$> [1..n], removing <$> [1..n]] where
  adding k = [add k (fromIntegral k), event k (fromIntegral (k * 2))]
  firing k = [event k (fromIntegral k)]
  removing k = [delete k]