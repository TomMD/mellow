{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
module Vision.Mellow where

import Vision.Freenect
import Vision.Image as I
import Vision.Primitive
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Time
import Data.Char (isSpace)
import Data.Word
import Control.Concurrent.MVar
import qualified Data.Vector.Storable as V
import System.IO
import System.Exit

import Control.DeepSeq
import Vision.Image.JuicyPixels (toJuicyRGBA, toFridayRGBA)
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Graphics.Gloss.Interface.IO.Game

renderInput, renderPsyc, renderOutlines :: RenderMethod
renderInput    = RenderCustom (depthToRGBA . inputFrame)
renderPsyc     = RenderCustom psycFrame
renderOutlines = RenderCustom outlinesFrame
renderBlurred  = RenderCustom (rgbaBlur 8 . psycFrame)

depthToRGBA :: Depth -> RGBA
depthToRGBA d = I.fromFunction (shape d) (\p -> let val = floor $ 255.0 * (fromIntegral (d!p) / 2047)
                                                  in RGBAPixel val val val 255)

-- XXX Make a config structure, use withKinect'
-- XXX add resolution, fps, etc, to this config

framePeriodMicroSec :: Int
framePeriodMicroSec = 100000

data Config s =
      Config { fnCfg      :: Freenect.Config
             , state0     :: s
             , resolution :: (Int,Int)
             , backgroundColor :: Color
             }

-- | @mellow state0 updateOp renderOp keyPress@ will continually call
-- updateOp with each new frame from a Kinect, call @renderOp@ to render
-- the frame using Gloss, and @keyPress@ to handle key presses.
mellow :: s -> (RGBA -> s -> IO s) -> (s -> Float -> IO RGBA) -> (Event -> Float -> s -> IO s) -> IO ()
mellow world updateOp renderOp keyPress handleEvent = do
  ref        <- newEmptyMVar :: IO (MVar Depth)
  worldRef   <- newMVar world
  let rdImg   = takeMVar ref
      wtImg x = tryPutMVar ref x

  -- Start a thread reading frames.
  forkIO $ withKinect (\_ i -> do threadDelay framePeriodMicroSec
                                  wtImg i)

  -- Start a thread that updates the state with each frame.
  forkIO $ forever $
            do i <- rdImg
               modifyMVar_ worldRef (updateOp i)

  playIO (FullScreen (640,480)) black 10 () (\_ -> readMVar worldRef >>= (fmap toPicture . renderOp)) (\e () -> modifyMVar_ worldRef $ handleEvent e time) (const return)

toPicture :: RGBA -> Picture
toPicture = scale 1 1 . fromImageRGBA8 . toJuicyRGBA

-- |
-- Example handle event:
--   * 'esc' quit
--   * 's' save the frame
--
-- @
-- handleEvent :: Event -> Float -> s -> (s -> RGBA) -> IO s
-- handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) _ _ _ = -- Quit
--   debug "Exiting." >> exitSuccess
-- handleEvent (EventKey (Char 's') Down _ _) t st rend =
--   do _ <- save JPG (show t ++ ".jpg") (rend st)
--      putStrLn $ "Saved " ++ show now
--      return st
-- handleEvent _ st                              = return st
-- @
