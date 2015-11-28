{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
module Mellow
  ( Config(..)
  , MellowCfg(..)
  , defaultCfg
  , depthToRGBA
  ) where

import Vision.Freenect as Freenect
import Vision.Image ()
import Vision.Image as I
import Vision.Image.JuicyPixels (toJuicyRGBA)
import Vision.Primitive
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as Lazy
import Data.IORef
import Data.Time
import Data.Char (isSpace)
import Data.Word
import Control.Concurrent.MVar
import qualified Data.Vector.Storable as V
import System.IO
import System.Exit
import Foreign.Storable ()

import Codec.Picture.Saving (imageToJpg)
import Codec.Picture (DynamicImage(..))
import Vision.Image.JuicyPixels (toJuicyRGBA, toFridayRGBA)
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Graphics.Gloss.Interface.IO.Game

depthToRGBA :: Depth -> RGBA
depthToRGBA d = I.fromFunction (shape d) (\p -> let val = floor $ 255.0 * (fromIntegral (d!p) / 2047)
                                                  in RGBAPixel val val val 255)

framePeriodMicroSec :: Int
framePeriodMicroSec = 100000

data MellowCfg s =
      MellowCfg { state0     :: s
                , resolution :: (Int,Int)
                , backgroundColor :: Color
                }

defaultCfg :: s -> MellowCfg s
defaultCfg s = MellowCfg s (640,480) black

-- | @mellow state0 updateOp renderOp keyPress@ will continually call
-- updateOp with each new frame from a Kinect, call @renderOp@ to render
-- the frame using Gloss, and @keyPress@ to handle key presses.
mellow :: s -> (Depth -> s -> IO s) -> (s -> IO RGBA) -> (Event -> s -> IO s) -> IO ()
mellow world updateOp renderOp keyPress = do
  ref        <- newEmptyMVar :: IO (MVar Depth)
  worldRef   <- newMVar world
  let rdImg   = takeMVar ref
      wtImg x = tryPutMVar ref x

  -- Start a thread reading frames.
  forkIO $ withKinect (\_ i -> do threadDelay framePeriodMicroSec
                                  wtImg i
                                  return ())

  -- Start a thread that updates the state with each frame.
  forkIO $ forever $
            do i <- rdImg
               modifyMVar_ worldRef (updateOp i)

  playIO (FullScreen (640,480)) black 10 ()
         (const $ readMVar worldRef >>= (fmap toPicture . renderOp))
         (\e () -> modifyMVar_ worldRef $ keyPress e)
         (const return)

toPicture :: RGBA -> Picture
toPicture = fromImageRGBA8 . toJuicyRGBA

-- |
-- Example handle event:
--   * 'esc' quit
--   * 's' save the frame
defaultEventHandler :: Event -> Float -> s -> (s -> RGBA) -> IO s
defaultEventHandler (EventKey (SpecialKey KeyEsc) _ _ _) _ _ _ = exitSuccess
defaultEventHandler (EventKey (Char 's') Down _ _) t st rend =
  do let jpg = toJuicyRGBA $ rend st
         bs  = imageToJpg 98 (ImageRGBA8 jpg)
     Lazy.writeFile (show t ++ ".jpg") bs
     return st
defaultEventHandler _ _ st _ = return st
