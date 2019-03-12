{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
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

-- | State Monad for simulation.
newtype EmitMeasureM a = EmitMeasureM (State ModeledLightIntensity a)
  -- deriving (Functor, Applicative, Monad, MonadState ModeledLightIntensity)

instance Functor EmitMeasureM
instance Applicative EmitMeasureM
instance Monad EmitMeasureM
instance MonadState ModeledLightIntensity EmitMeasureM

-- | Real instances of emit and measure. Here, Since the example is imaginary
-- and we have no actual devices implementation, for demonstration purposes
-- human interaction is expected.
instance MonadEmitMeasure IO where
  emit = print
  measure = read <$> getLine

-- | Simulated instances of emit and measure.
-- instance MonadEmitMeasure (State ModeledLightIntensity) where
instance MonadEmitMeasure EmitMeasureM where
  emit = put . (ModeledLightIntensity . (* 1.5) . fromIntegral)
  measure = (\(ModeledLightIntensity x) -> realToFrac x) <$> get

initialModel :: ModeledLightIntensity
initialModel = ModeledLightIntensity 0.0

main :: IO ()
main =
  print $ runState ((\(EmitMeasureM s) -> s) functionWithActions) initialModel

-- | Function that uses actions of emit and measure within a shared context.
functionWithActions :: MonadEmitMeasure m => m MeasuredLightIntensity
functionWithActions = do
  m1 <- measure
  emit (round m1 + 1)
  m2 <- measure
  emit (round m2 + 2)
  measure
