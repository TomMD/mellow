{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE MultiWayIf         #-}
module Mellow
  ( -- * Types
    MellowCfg(..)
  , Depth
  , defaultCfg
    -- * Main Interface
  , mellow, mellowWith
  , defaultEventHandler
    -- * Friday re-exports
  , Z(..), shape, (:.)(..), DIM2
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
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Graphics.Gloss.Interface.IO.Game

data MellowCfg s =
      MellowCfg { world           :: s
                , resolution      :: (Int,Int)
                , backgroundColor :: Color
                , framerate       :: Int
                , updateOp        :: Depth -> s -> IO s
                , renderOp        :: s -> IO RGBA
                , eventOp         :: Event -> s -> IO s
                }

defaultCfg :: s                         -- ^ Initial state
           -> (Depth -> s -> IO s)      -- ^ Integrate new depth frame
           -> (s -> IO RGBA)            -- ^ Render state into an RGBA image
           -> (Event -> s -> IO s)      -- ^ Event handler
           -> MellowCfg s
defaultCfg s = MellowCfg s (640,480) black 20

mellowWith :: MellowCfg s -> IO ()
mellowWith (MellowCfg {..}) = do
  ref        <- newEmptyMVar :: IO (MVar Depth)
  worldRef   <- newMVar world
  let rdImg   = takeMVar ref
      wtImg x = tryPutMVar ref x >> return ()

  -- Start a thread reading frames.
  _ <- forkIO $ withKinect (\_ i -> do wtImg i
                                       return ())

  -- Start a thread that updates the state with each frame.
  _ <- forkIO $ forever $
                do i <- rdImg
                   modifyMVar_ worldRef (updateOp i)

  playIO ({- FullScreen -} InWindow "Test" resolution (500,0)) backgroundColor framerate ()
         (const $ readMVar worldRef >>= (fmap toPicture . renderOp))
         (\e () -> modifyMVar_ worldRef $ eventOp e)
         (const return)

-- | @mellow state0 updateOp renderOp keyPress@ will continually call
-- updateOp with each new frame from a Kinect, call @renderOp@ to render
-- the frame using Gloss, and @keyPress@ to handle key presses.
mellow :: s -> (Depth -> s -> IO s) -> (s -> IO RGBA) -> (Event -> s -> IO s) -> IO ()
mellow world updateOp renderOp keyPress =
  mellowWith (defaultCfg world updateOp renderOp keyPress)

toPicture :: RGBA -> Picture
toPicture = fromImageRGBA8 . toJuicyRGBA

-- |
-- Example handle event:
--   * 'esc' quit
--   * 's' save the frame
defaultEventHandler :: (s -> IO RGBA) -> Event -> s -> IO s
defaultEventHandler _ (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
defaultEventHandler rend (EventKey (Char 's') Down _ _) st =
  do jpg <- toJuicyRGBA <$> rend st
     let bs = imageToJpg 98 (ImageRGBA8 jpg)
     t <- getCurrentTime
     Lazy.writeFile (show t ++ ".jpg") bs
     return st
defaultEventHandler _ _ st = return st
