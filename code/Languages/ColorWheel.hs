module Languages.ColorWheel where

import qualified Data.Set as S
import Data.Set (Set)
import Diagram
import Diagrams.Prelude hiding (ix, E, value)
import Diagrams.Backend.Rasterific
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv, hsvView, value)
import Data.Colour.RGBSpace.HSL (hsl, hslView, lightness)
import Debug.Trace
import Data.Typeable
import Data.Function (on)
import Data.List

newtype ColorName = ColorName
  { unColorName :: Colour Double
  }
  deriving (Eq, Show, Typeable)

instance Ord ColorName where
  compare = on compare show

instance IsName ColorName

newtype ColorSet = ColorSet
  { getColorSet :: Set ColorName
  }
  deriving (Eq, Ord, Show)

sHSV :: (Floating b, RealFrac b) => b -> b -> b -> Colour b
sHSV h s v = uncurryRGB sRGB $ hsv h s v

sHSL :: (Floating b, RealFrac b) => b -> b -> b -> Colour b
sHSL h s v = uncurryRGB sRGB $ hsl h s v

wedg y dir arcwidth =
  annularWedge
    (fromIntegral y + 1)
    (0.0001 + fromIntegral y)
    dir
    (arcwidth @@ turn)

colorSet :: Diagram B -> ColorSet
colorSet
  = ColorSet
  . S.fromList
  . (toListOf eachName =<<)
  . fmap fst
  . names

nearestColor :: ColorSet -> Colour Double -> Name
nearestColor (ColorSet s) (toSRGB -> RGB r g b)
  = toName
  $ head
  $ sortOn
      ( (\(RGB r1 g1 b1) -> (r - r1) ** 2 + (g - g1) ** 2 + (b - b1) ** 2)
        . toSRGB
        . unColorName
      )
  $ S.toList s

showOne :: (Colour Double -> Colour Double) -> Colour Double -> Diagram B -> Diagram B
showOne f i d =
  let cs = colorSet d
   in connect'' i (nearestColor cs i) (nearestColor cs (f i)) d

connect'' :: Colour Double -> Name -> Name -> Diagram B -> Diagram B
connect'' col n1 n2 d
  | n1 == n2 = d
  | otherwise =
      let acol = withOpacity (contrastify 0.75 col) 1
       in
        connect'
          (def
            & shaftStyle %~ lcA acol . lw veryThick
            & headStyle %~ lcA acol . fcA acol
            & arrowShaft .~
                cubicSpline False (fmap p2 [(0, 0), (1, 0.4), (2, 0)])
          ) n1 n2 d

contrastify :: Double -> Colour Double -> Colour Double
contrastify a c =
  case value (toSRGB c) < 0.51 of
    True -> saturate 0.5 $ blend (1 - a) c white
    False -> darken a c

showAll :: (Colour Double -> Colour Double) -> Diagram B -> Diagram B
showAll f d =
  let cs = colorSet d
   in foldr (\cn@(ColorName c) -> connect'' c (toName cn) $ nearestColor cs $ f c) d
    $ S.toList
    $ getColorSet cs

rotateHue :: (Ord a, RealFloat a) => a -> Colour a -> Colour a
rotateHue a (hsvView . toSRGB -> (h, s, v)) = sHSV (h + a) s v

saturate :: (Ord a, RealFloat a) => a -> Colour a -> Colour a
saturate a (hsvView . toSRGB -> (h, s, v)) = sHSV h (s + a) v

envalue :: (Ord a, RealFloat a) => a -> Colour a -> Colour a
envalue a (hsvView . toSRGB -> (h, s, v)) = sHSV h s (v + a)

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
        ang = rotateBy deg $ V2 1 0
        dir = (dirBetween 0 $ P $ rotateBy deg $ V2 1 0)
        off = (pathCentroid $ wedg y dir arcwidth) ^. _Point
    pure
      $ lcA transparent
      $ fc col
      $ moveOriginBy (negate off)
      $ named (ColorName col)
      $ moveOriginBy off
      $ wedg y dir arcwidth

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
