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
runApp app b0 inputFrames = do 
  runFrame <- runSetup setupApp buildRead
  traverse runFrame inputFrames 
  where 
    setupApp input = do
      inputB <- hold b0 pulseB
      app $ AppIn inputB inputE
        where (pulseB, inputE) = fanThese input


    buildRead (AppOut outB outE) = do
        hnd <- subscribeEvent outE
        return $ liftA2 (,) (sample outB) (readEvent' hnd)

runSetup :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t a -> PerformEventT t m out)
       -> (out -> m (ReadPhase m b))
       -> IO (Maybe a -> IO [b])
runSetup app buildRead = runSpiderHost $ do
  (inputEvent, triggerRef) <- newEventWithTriggerRef
  (out, FireCommand fire) <- hostPerformEventT $ app inputEvent
  readPhase <- buildRead out

  mTrigger <- readRef triggerRef
  return $ \mInput ->  runSpiderHost $
    fire (maybeTrigger mTrigger mInput) readPhase
  where
    maybeTrigger mTrigger = fromMaybe [] .
      liftA2 (\trigger a -> [trigger :=> Identity a]) mTrigger

subscribeAndRead :: MonadReflexHost t m => Event t a -> m (ReadPhase m (Maybe a))
subscribeAndRead e = readEvent' <$> subscribeEvent e
  
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


  