{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# OPTIONS_GHC -w #-}
module Main where

import Mellow
import Vision.Image as I
import qualified Vision.Image.Filter.Internal as I
import Vision.Primitive
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Time
import Data.Char (isSpace)
import Data.Maybe (listToMaybe)
import Data.Word
-- import Vision.Image.Storage.DevIL -- requires friday-devil too
import Control.Concurrent.MVar
import qualified Data.Vector.Storable as V
import System.IO
import System.Exit

import Control.DeepSeq
import Vision.Image.JuicyPixels (toJuicyRGBA, toFridayRGBA)
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Graphics.Gloss.Interface.IO.Game

-- | The world consists of the psycState and a render method.  The
-- PsycState includes the wacky frame, input frame, processing frames,
-- background elimination process, colors, information for deleting frame
-- older than some number of steps.
-- 'renderMethod' just selects one of these many states to render
data World = World { psycState     :: PsycState Depth
                   , renderMethod  :: RenderMethod
                   }

newtype RenderMethod = RenderCustom (PsycState Depth -> RGBA)

renderInput, renderPsyc, renderOutlines :: RenderMethod
renderInput    = RenderCustom (depthToRGBA . inputFrame)
renderPsyc     = RenderCustom psycFrame
renderOutlines = RenderCustom outlinesFrame
renderBlurred  = RenderCustom (rgbaBlur 8 . psycFrame)

depthToRGBA :: Depth -> RGBA
depthToRGBA d = I.fromFunction (shape d) (\p -> let val = floor $ 255.0 * (fromIntegral (d!p) / 2047)
                                                in RGBAPixel val val val 255)

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 640 :. 480) (const (RGBAPixel 0 0 0 0))

main :: IO ()
main = mellow (initialWorld []) update (return . render) handleEvent

update :: Depth -> World -> IO World
update i w =
  let new = psyc (psycState w) i
  in new `deepseq` return w { psycState = new }

render :: World -> RGBA
render (World {..}) | RenderCustom f <- renderMethod =
  case psycState of
    PSInit {} -> blackRGBA
    PS {}     -> horizontalFlip (f psycState)

-- | Maximum number of frames held for funky rendering effect.
maxCaps :: Int
maxCaps = 6

initialWorld :: [Depth] -> World
initialWorld xs =
    let psyc0 = initialPsycState xs
    in World psyc0 renderBlurred

initialPsycState :: [Depth] -> PsycState Depth
initialPsycState [] = PSInit []
initialPsycState xs =
    let cleans = fmap clean xs
        clean  :: Depth -> Depth
        clean  = I.map (\p -> if p < 100 then 4095 else p)
    in maybe (error "Impossible") id $ initPsyc (depthFilter cleans) maxCaps xs

depthFilter :: [Depth] -> Depth -> Grey
depthFilter ds@(d0:_) = depthMask `seq` \t ->
    let bods = getBodies t
        e    = erode 4 bods   :: Grey
        d    = dilate 4 bods
        dBig = dilate 6 d
    in dBig .- d
 where
  (Z :. h :. w) = shape d0
  (.-) :: Grey -> Grey -> Grey
  (.-) o i = fromFunction (shape o) (\p -> if i!p /= 0 then 0 else o!p)
  getBodies :: Depth -> Grey
  getBodies testImage =
      I.fromFunction (shape testImage) (\p -> if (testImage ! p) < (depthMask ! p) then 255 else 0)
  -- Min: best so far, square artifacts persist
  depthMask :: Depth
  !depthMask = manifest $ I.map (\x -> if x < 100 then 4095 else (floor . (0.97 *) . fromIntegral $ x) ) $ minPF (Z :. 25 :. 25) $ foldl1 (zipImage min) ds

  -- Harmonic: works so so
  -- !depthMask = I.fromFunction (shape d0) (\p -> (0.9 *) $ harmonicMean $ V.map fromIntegral $ V.fromList $ fmap (!p) ds)

zipImage :: (Word16 -> Word16 -> Word16) -> Depth -> Depth -> Depth
zipImage f a b = fromFunction (shape a) (\p -> f (a ! p) (b ! p))

--------------------------------------------------------------------------------
--  Event Handling (key presses)

--   * 'esc' quit
--   * 'r' reset the background
--   * 's' save the frame
--   * 'p' display Psychedelic image
--   * 'i' display input image
--   * 'o' display outline image
handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) st = exitSuccess
handleEvent (EventKey (Char 's') Down _ _) st =
  do -- now <- getCurrentTime
     -- let nowString = filter (not . isSpace) (show now)
     -- if Devil is imported _ <- save JPG (nowString ++ ".jpg") (render st)
     -- putStrLn $ "Saved " ++ show now
     return st
handleEvent (EventKey (Char 'i') Down _ _) st = return st { renderMethod = renderInput    }
handleEvent (EventKey (Char 'o') Down _ _) st = return st { renderMethod = renderOutlines }
handleEvent (EventKey (Char 'p') Down _ _) st = return st { renderMethod = renderPsyc     }
handleEvent (EventKey (Char 'b') Down _ _) st = return st { renderMethod = renderBlurred  }
handleEvent _ st                              = return st


--------------------------------------------------------------------------------
--  Psychedelic Image Creation

-- PsycState contains enough information to
-- 1) Ingest new images by removing the background and performing contour tracing.
-- 2) Delete old outlines from the rendered frame without re-drawing all of history.
-- 3) The frame as it exists today, for adding new overlays and deleting old.
data PsycState inImg
       = PSInit [inImg]
       | PS { getOutlines   :: inImg -> Grey        -- Removes background, yielding row contours of an image
            , history       :: [(Grey,RGBAPixel)]   -- oldest-to-newest queue of contours for use in deletion
            , inputFrame    :: inImg                -- Most recent input frame
            , psycFrame     :: RGBA                 -- Current psycedelic image before final processing (for deleting old outlines)
            , outlinesFrame :: RGBA
            , colors        :: [RGBAPixel]          -- Infinite list of colors for drawing
            }

-- The state retains N frames, each new frame is given the next color in the list.
defaultColors :: [RGBAPixel]
defaultColors = cycle [RGBAPixel 0x3f 0x63 0xad 0xff
                      ,RGBAPixel 0x40 0x64 0xae 0xff
                      ,RGBAPixel 0xae 0x09 0xea 0xff
                      ,RGBAPixel 0xd1 0x03 0xe2 0xff
                      ,RGBAPixel 0xea 0x2f 0xa7 0xff
                      ,RGBAPixel 0xf4 0x72 0x5a 0xff
                      ,RGBAPixel 0xfa 0xae 0x1c 0xff
                      ,RGBAPixel 0xee 0xfe 0x84 0xff
                      ,RGBAPixel 0x75 0xf0 0x7b 0xff
                      ,RGBAPixel 0x0a 0xd0 0x6e 0xff
                      ,RGBAPixel 0x07 0xa9 0x80 0xff
                      ,RGBAPixel 0x2b 0x79 0xa0 0xff
                      ]

instance NFData a => NFData (PsycState a) where
    rnf (PS a b c r o d) = r `deepseq` o `deepseq` a `seq` b `seq` ()
    rnf _                = ()


-- Make a PsycState based on background samples and number of history frames to retain
initPsyc :: MaskedImage inImg => (inImg -> Grey) -> Int -> [inImg] -> Maybe (PsycState inImg)
initPsyc _ _ []  = Nothing
initPsyc getOutlines nr xs@(firstX:_) =
    let history     = replicate nr (frame0,background)
        frame0      = fromFunction sz (const 0)
        psycFrame   = fromFunction sz (const (RGBAPixel 0 0 0 0))
        inputFrame  = firstX
        colors      = defaultColors
    in Just PS { .. }
  where
      contours :: Grey -> Grey
      contours i0 =
                   let i  = erode  3  i0 :: Grey
                       d3 = dilate 4  i  :: Grey
                       d7 = dilate 3  d3 :: Grey
                       v3 p = d3 ! p
                       v7 p = d7 ! p
                   in fromFunction sz (\idx -> if v7 idx /= 0 then (if v3 idx /= 0 then 0 else 255) else 0)
      sz = shape (head xs)

-- Update the PsycState with a new image, yielding a new 'frame' for
-- rendering as well as updating the 'history'.
ingest :: PsycState Depth -> Depth -> PsycState Depth
ingest (PSInit xs) img | length xs < neededInitFrames = PSInit (img:xs)
                       | otherwise = ingest (initialPsycState xs) img
ingest orig@(PS {..}) img = orig { history       = hist
                                 , psycFrame     = compositeFrame
                                 , inputFrame    = img
                                 , outlinesFrame = I.map (\p -> let x = fromIntegral p in RGBAPixel x x x x) os
                                 , colors        = newColors
                                 }
  where
  os                    = getOutlines img
  hist                  = history ++ [(os,currColor)]
  compositeFrame        = drawPsychedelic os currColor psycFrame
  (currColor:newColors) = colors

  drawPsychedelic :: Grey -> RGBAPixel -> RGBA -> RGBA
  drawPsychedelic os color@(RGBAPixel r g b _) frame =
    fromFunction (shape frame)
                 (\pnt -> case os ! pnt of
                             0    -> frame ! pnt
                             1    -> background
                             o    -> color)



rgbaBlur :: Int -> RGBA -> RGBA
rgbaBlur i = onComponents (blur i)
 where
 onComponents :: (Grey -> Grey) -> RGBA -> RGBA
 onComponents f img = fromFunction (shape img) (\p -> RGBAPixel (fromIntegral $ r!p) (fromIntegral $ g!p) (fromIntegral $ b!p) (rgbaAlpha (img!p)))
   where
     (redChan,greenChan,blueChan) = (I.map (fromIntegral . rgbaRed) img, I.map (fromIntegral . rgbaGreen) img, I.map (fromIntegral . rgbaBlue) img)
     r = f redChan
     g = f greenChan
     b = f blueChan

-- | The number of frames needed to compute a useful depth mask to perform
-- background removal (depends on hardware and stability of mounting).
neededInitFrames :: Int
neededInitFrames = 300

-- | Ingest an image and age-out old images in one step.
psyc :: PsycState Depth -> Depth -> PsycState Depth
psyc (PSInit xs) i | length xs < neededInitFrames = PSInit (i:xs)
                   | otherwise = psyc (initialPsycState xs) i
psyc st img = garbageCollect (ingest st img)
 where
  -- The early version just take the most-recent X frames, eventually this
  -- should be a time-driven collection so frame-rate doesn't have such an
  -- impact.
  garbageCollect :: NFData inImg => PsycState inImg -> PsycState inImg
  garbageCollect ps@(PS {..}) =
    let newPF = deleteOne (listToMaybe history) psycFrame
    in newPF `deepseq` ps { history   = (drop 1 history)
                          , psycFrame = newPF }

  deleteOne :: Maybe (Grey,RGBAPixel) -> RGBA -> RGBA
  deleteOne Nothing f           = f
  deleteOne (Just (ps,color)) f =
      let mk pnt = if f ! pnt == color && (ps ! pnt /= 0) -- XXX delete color regardless of alpha
                       then background
                       else f ! pnt
      in fromFunction (shape f) mk

background :: RGBAPixel
background = RGBAPixel 0 0 0 0

minPF :: (FromFunction src, Image src, Integral (FromFunctionPixel src), FromFunctionPixel src ~ ImagePixel src, SeparatelyFiltrable src src (ImagePixel src), Integral (ImagePixel src)) => Size -> src -> src
minPF sz img = I.apply (minFilter sz) img
{-# INLINE minPF #-}

-- | Computes the minimum of a region
--
-- This is similar to 'blur' but with a rectangular kernel and a 'Fractional'
-- result.
minFilter :: (Integral src)
     => Size -> I.SeparableFilter src () src src
minFilter size =
    I.Filter size I.KernelAnchorCenter (I.SeparableKernel vert horiz) (\_ _ -> ())
           (I.FilterFold (const 4095)) post I.BorderReplicate
  where
    vert  _ _ !val  !acc = min acc val
    horiz _ _ !acc' !acc = min acc acc'
    post _ _ _ !acc  = acc
{-# INLINE minFilter #-}
