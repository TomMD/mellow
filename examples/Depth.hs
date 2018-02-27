{-# LANGUAGE MultiWayIf #-}
import Mellow                   -- The main 'Mellow' module
import Vision.Image as I        -- The Friday library for image manipulation

main :: IO ()
main =
  mellow blackRGBA                         -- Black image to start
         (\d _ -> return (depthToRGBA d))  -- RGBA from 11-bit depth
         return                            -- Noop renders
         (defaultEventHandler return)
              -- The default event handler allows us to quit using 'esc'
              -- as well as save screen captures with 's'.

depthToRGBA :: Depth -> RGBA
depthToRGBA =
  I.map                 -- Friday image manipulation map operation
      (\val ->
          let r = norm True  (fromIntegral (val-200) / 800)
              g = norm True  (fromIntegral (500 + abs (val-700)) / 1000)
              b = norm False (fromIntegral (val - 900) / 1000)
          in RGBAPixel r g b 255)
  where
   norm b = floor . (255 *) . (if b then (1 - ) else id) . max 0 . min one
   one    = 1 :: Double

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 480 :. 640) (const (RGBAPixel 0 0 0 0))
