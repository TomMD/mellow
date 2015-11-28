{-# LANGUAGE MultiWayIf #-}
import Mellow                   -- The main 'Mellow' module
import Vision.Image as I        -- The Friday library for image manipulation

type State = Maybe RGBA

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
      (\val -> RGBAPixel (oper val 0 950) (oper val 800 1200) (oper val 1000 2100) 255)
  where oper v n x =
          let v' | v > x = 0
                 | v < n = 0
                 | otherwise = fromIntegral (v - n)
          in floor $ 255 * (v' / 700 :: Double)

blackRGBA :: RGBA
blackRGBA = I.fromFunction (Z :. 480 :. 640) (const (RGBAPixel 0 255 0 255))
