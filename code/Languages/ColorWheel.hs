module Languages.ColorWheel where

import           Data.Colour.RGBSpace.HSV (hsvView, value)
import           Data.Colour.SRGB.Linear (rgb, toRGB)
import           Data.Function (on)
import           Data.List (sortOn)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable
import           Diagram
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude hiding (ix, E, value, deg)
import           Types

wedg y dir arcwidth =
  annularWedge
    (fromIntegral y + 1)
    (0.0001 + fromIntegral y)
    dir
    (arcwidth @@ turn)

rotateHue :: RealFloat a => a -> Colour a -> Colour a
rotateHue a (hsvView . toRGB -> (h, s, v)) = sHSV (h + a) s v

envalue :: RealFloat a => a -> Colour a -> Colour a
envalue a (hsvView . toRGB -> (h, s, v)) = sHSV h s (v + a)

colorWheel :: Int -> Int -> (Double -> Double -> Colour Double) -> Diagram B
colorWheel m n f =
  mconcat $ do
    let vs = zip [id @Int 0..] $ [id @Int 1 .. m]
    let hues = zip [id @Int 0..] [id @Int 1 .. n]
    (y, v) <- vs
    (ix, hue) <- hues
    let col = f (fromIntegral hue / fromIntegral n * 360)
                (fromIntegral v / (fromIntegral $ length vs))
    let arcwidth = 1 / fromIntegral n
        deg = arcwidth * fromIntegral ix
        dir = (dirBetween 0 $ P $ rotateBy deg $ V2 1 0)
        off = (pathCentroid $ wedg y dir arcwidth) ^. _Point
    pure
      $ lcA transparent
      $ fc col
      $ moveOriginBy (negate off)
      $ named (ColorName col)
      $ moveOriginBy off
      $ wedg y dir arcwidth

-- yo =
--   __design ("",[],[("design","code/Languages/ColorWheel.hs")]) "colorWheel 5 10 $ \\h v -> sHSV h v 1\n" "./.design-tools/5283047251292810106" $
--      colorWheel 5 10 $ \h v -> sHSV h v 1

-- main :: IO ()
-- main
--   = renderRasterific "/tmp/diagram.png" (dims 300)
--   $ showAll
--       (rotateHue $ -30)
--       -- (darken 0.5)
--       -- (saturate $ -0.5)
--   $ colorWheel 5 10
--       $ \h v -> sHSV h v 1
--       -- $ \h v -> sHSV h 1 v
