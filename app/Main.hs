module Main where

import Control.Monad.State


-- | Screen image data (for example, it can be Picture from Gloss).
-- For simplicity of the example it's just Float.
type ScreenImage = Float

-- | Camera shot image. Different types to distinguish more easily from
-- ScreenImage.
type ShotImage = Int

-- | Draw image on the screen.
type DrawImage = ScreenImage -> IO ()

-- | Take a shot of the screen.
type TakeShot = IO ShotImage


initialScreenImage :: ScreenImage
initialScreenImage = 0.0

main :: IO ()
main = do
  _ <- runStateT ioSimulationT initialScreenImage
  return ()

ioSimulationT :: StateT ScreenImage IO ()
-- ioSimulationT = liftIO $ realFunctionUsingIO (\_ -> print 1.0) (return 5)
ioSimulationT = do
  -- draw <- simulateDraw
  shot <- simulateShot
  liftIO $ realFunctionUsingIO (\_ -> print 1.0) (liftIO (return shot))

-- | Draw image on screen simulation.
simulateDraw :: ScreenImage -> StateT ScreenImage IO ()
simulateDraw image = put image

-- | Take shot of the screen simulation.
simulateShot :: StateT ScreenImage IO ShotImage
simulateShot = round <$> get

-- | Produces new ScreenImage from ShotImage.
calibrateImage :: ShotImage -> ScreenImage
calibrateImage = (+) 1 . fromIntegral

-- | Function that uses functions with real devices. We cannot touch
-- implementation of this function, that's the point.
realFunctionUsingIO :: DrawImage -> TakeShot -> IO ()
realFunctionUsingIO drawImage takeShot = do
  shotImage1 <- takeShot
  print shotImage1
  drawImage (calibrateImage shotImage1)
  shotImage2 <- takeShot
  print shotImage2
  drawImage (calibrateImage shotImage2)
  shotImage3 <- takeShot
  print shotImage3
