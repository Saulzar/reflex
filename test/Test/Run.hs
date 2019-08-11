{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Test.Run where

import Control.Monad  
import Control.Monad.Trans
import Control.Monad.Ref
import Control.Applicative
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.These
import Data.Maybe

import Reflex
import Reflex.Host.Class

data AppIn t b e = AppIn
  { _appIn_behavior :: Behavior t b
  , _appIn_event :: Event t e
  }

data AppOut t b e = AppOut
  { _appOut_behavior :: Behavior t b
  , _appOut_event :: Event t e
  }


runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
  => (AppIn t bIn eIn -> PerformEventT t m (AppOut t bOut eOut))
  -> bIn
  -> [Maybe (These bIn eIn)]
  -> IO [[(bOut, Maybe eOut)]]
runApp app b0 input = do 
  runFrame <- setupApp setup
  traverse runFrame input
  where 
    setup input = do
      inputB <- hold b0 pulseB
      (AppOut outB outE)  <- app $ AppIn inputB inputE
      hnd <- PerformEventT . lift $ subscribeEvent outE
      return $ liftA2 (,) (sample outB) (readEvent' hnd)
        where (pulseB, inputE) = fanThese input

setupApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t a -> PerformEventT t m (ReadPhase m b))
       -> IO (Maybe a -> IO [b])
setupApp app = runSpiderHost $ do
  (inputEvent, triggerRef) <- newEventWithTriggerRef
  (readPhase, FireCommand fire) <- hostPerformEventT $ app inputEvent
  mTrigger <- readRef triggerRef
  return $ \mInput ->  runSpiderHost $
    fire (maybeTrigger mTrigger mInput) readPhase

  where
    maybeTrigger mTrigger = fromMaybe [] .
      liftA2 (\trigger a -> [trigger :=> Identity a]) mTrigger


runApp' :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Event t eOut))
        -> [Maybe eIn]
        -> IO [[Maybe eOut]]
runApp' app input = do
  let app' = fmap (AppOut (pure ())) . app
  map (map snd) <$> runApp (app' . _appIn_event) () (map (fmap That) input)

runAppB :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
        => (Event t eIn -> PerformEventT t m (Behavior t bOut))
        -> [Maybe eIn]
        -> IO [[bOut]]
runAppB app input = do
  let app' = fmap (flip AppOut never) . app
  map (map fst) <$> runApp (app' . _appIn_event) () (map (fmap That) input)


  