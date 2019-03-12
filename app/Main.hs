{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.State

-- | Modeled physical light intensity.
newtype ModeledLightIntensity = ModeledLightIntensity Double
  deriving Show

-- | Emitter intensity discrete value.
type EmitterLightIntensity = Int

-- | Sensor measured value of light intensity.
type MeasuredLightIntensity = Float

-- | Actions class of emit and measure within shared context.
class Monad m => MonadEmitMeasure m where
  -- | Emit light of specified discrete intensity value (emitter is capable of).
  emit :: EmitterLightIntensity -> m ()
  -- | Measure physical light intensity.
  measure :: m MeasuredLightIntensity

-- | Simulated instances of emit and measure.
instance MonadEmitMeasure (State ModeledLightIntensity) where
  emit = put . (ModeledLightIntensity . (* 1.5) . fromIntegral)
  measure = (\(ModeledLightIntensity x) -> realToFrac x) <$> get

initialModel :: ModeledLightIntensity
initialModel = ModeledLightIntensity 0.0

main :: IO ()
main =
  print $ runState functionWithActions initialModel

-- | Function that uses actions of emit and measure within a shared context.
functionWithActions :: MonadEmitMeasure m => m MeasuredLightIntensity
functionWithActions = do
  m1 <- measure
  emit (round m1 + 1)
  m2 <- measure
  emit (round m2 + 2)
  measure
