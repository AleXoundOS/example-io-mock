{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.State


-- | Screen image data (for example, it can be Picture from Gloss).
-- For simplicity of the example it's just Float.
type ScreenImage = Int

-- | Camera shot image. Different types to distinguish more easily from
-- ScreenImage.
type ShotImage = Float

-- | Draw image on the screen.
type DrawImage m = ScreenImage -> m ()

-- | Take a shot of the screen.
type TakeShot m = m ShotImage

-- | Операции отображения и съёмки в общем контексте. Полезно для симуляции IO
-- при тестировании.
class Monad m => MonadDrawShot m where
  drawImage :: DrawImage m
  takeShot :: TakeShot m


instance MonadDrawShot IO where
  drawImage = print
  takeShot = read <$> getLine

instance MonadDrawShot (State ScreenImage) where
  drawImage = put
  takeShot = fromIntegral <$> get

initialScreenImage :: ScreenImage
initialScreenImage = 0

main :: IO ()
main = do
  _ <- runStateT ioSimulationT initialScreenImage
  return ()

test = do
  runState (realFunctionUsingIO drawImage takeShot) initialScreenImage

ioSimulationT :: StateT ScreenImage IO ()
-- ioSimulationT = liftIO $ realFunctionUsingIO (\_ -> print 1.0) (return 5)
ioSimulationT = do
  -- draw <- simulateDraw
  shot <- simulateShot
  liftIO $ realFunctionUsingIO (\_ -> print 1.0) (liftIO (return shot))

-- | Draw image on screen simulation.
simulateDraw :: MonadDrawShot m => ScreenImage -> StateT ScreenImage m ()
simulateDraw image = put image

-- | Take shot of the screen simulation.
simulateShot :: MonadDrawShot m => StateT ScreenImage m ShotImage
simulateShot = fromIntegral <$> get

-- | Produces new ScreenImage from ShotImage.
calibrateImage :: ShotImage -> ScreenImage
calibrateImage = (+) 1 . round

-- | Function that uses functions with real devices. We cannot touch
-- implementation of this function, that's the point.
realFunctionUsingIO :: MonadDrawShot m => DrawImage m -> TakeShot m -> m ()
realFunctionUsingIO drawImage' takeShot' = do
  shotImage1 <- takeShot'
  drawImage' (calibrateImage shotImage1)
  shotImage2 <- takeShot'
  drawImage' (calibrateImage shotImage2)
  shotImage3 <- takeShot'
  drawImage' (calibrateImage shotImage3)
