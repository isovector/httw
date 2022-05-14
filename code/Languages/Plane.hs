module Languages.Plane where

import Diagram
import Diagrams.Prelude hiding (ix, E, value)
import Diagrams.Backend.Rasterific
import Prelude hiding (tan)
import Data.Colour.Names
import Data.List
import Types

plane :: Int -> Int -> (Double -> Double -> Colour Double) -> Diagram B
plane m n f =
  center $ vcat $ do
    y <- [1 .. n]
    pure $ hcat $ do
      x <- [1 .. m]
      let xx = fromIntegral x / fromIntegral m
          yy = fromIntegral y / fromIntegral n
          col = f xx (1 - yy)
      pure
        $ named (ColorName col)
        $ lc col
        $ fc col
        $ rect 1 1

net :: Int -> Int -> (Double -> Double -> Double -> Colour Double) -> Diagram B
net m n f = hsep 1
  [ plane m n $ \b r -> f r 0 (1 - b)
  , center $ vsep 1
      [ plane m n $ \g b -> f 1 g b
      , plane m n $ \g r -> f r g 0
      , plane m n $ \g b -> f 0 g (1 - b)
      ]
  , plane m n $ \b r -> f r 1 b
  , plane m n $ \g r -> f r (1 - g) 1
  ]


linear :: [(String, Colour Double)] -> Diagram B
linear
  = hcat
  . fmap (\(s, c) ->
      let r = lc c $ fc c $ square 1.5
       in withEnvelope r $ atop
            r
            $ moveOriginBy (V2 1 0.4) $ rotateBy (-0.16) $ moveOriginBy (-V2 1.5 0) $ mkText s
         )

mkText :: String -> Diagram B
mkText = texterific


-- main :: IO ()
-- main
--   = renderRasterific "/tmp/diagram.png" (dims 300) $ net 15 15 sRGB
--   --     $ \(round . (* m) -> x) (round . (* n) -> y) -> namedColors !! (y * m + x)
--   --     -- $ \h v -> sHSV h 1 v
--   -- where
--   --   m = 1
--   --   n = 2

namedColors :: (Ord a, Floating a) => [(String, Colour a)]
namedColors =
 [ ("aliceblue", aliceblue)
 , ("antiquewhite", antiquewhite)
 , ("aqua", aqua)
 , ("aquamarine", aquamarine)
 , ("azure", azure)
 , ("beige", beige)
 , ("bisque", bisque)
 , ("black", black)
 , ("blanchedalmond", blanchedalmond)
 , ("blue", blue)
 , ("blueviolet", blueviolet)
 , ("brown", brown)
 , ("burlywood", burlywood)
 , ("cadetblue", cadetblue)
 , ("chartreuse", chartreuse)
 , ("chocolate", chocolate)
 , ("coral", coral)
 , ("cornflowerblue", cornflowerblue)
 , ("cornsilk", cornsilk)
 , ("crimson", crimson)
 , ("cyan", cyan)
 , ("darkblue", darkblue)
 , ("darkcyan", darkcyan)
 , ("darkgoldenrod", darkgoldenrod)
 , ("darkgray", darkgray)
 , ("darkgreen", darkgreen)
 , ("darkgrey", darkgrey)
 , ("darkkhaki", darkkhaki)
 , ("darkmagenta", darkmagenta)
 , ("darkolivegreen", darkolivegreen)
 , ("darkorange", darkorange)
 , ("darkorchid", darkorchid)
 , ("darkred", darkred)
 , ("darksalmon", darksalmon)
 , ("darkseagreen", darkseagreen)
 , ("darkslateblue", darkslateblue)
 , ("darkslategray", darkslategray)
 , ("darkslategrey", darkslategrey)
 , ("darkturquoise", darkturquoise)
 , ("darkviolet", darkviolet)
 , ("deeppink", deeppink)
 , ("deepskyblue", deepskyblue)
 , ("dimgray", dimgray)
 , ("dimgrey", dimgrey)
 , ("dodgerblue", dodgerblue)
 , ("firebrick", firebrick)
 , ("floralwhite", floralwhite)
 , ("forestgreen", forestgreen)
 , ("fuchsia", fuchsia)
 , ("gainsboro", gainsboro)
 , ("ghostwhite", ghostwhite)
 , ("gold", gold)
 , ("goldenrod", goldenrod)
 , ("gray", gray)
 , ("grey", grey)
 , ("green", green)
 , ("greenyellow", greenyellow)
 , ("honeydew", honeydew)
 , ("hotpink", hotpink)
 , ("indianred", indianred)
 , ("indigo", indigo)
 , ("ivory", ivory)
 , ("khaki", khaki)
 , ("lavender", lavender)
 , ("lavenderblush", lavenderblush)
 , ("lawngreen", lawngreen)
 , ("lemonchiffon", lemonchiffon)
 , ("lightblue", lightblue)
 , ("lightcoral", lightcoral)
 , ("lightcyan", lightcyan)
 , ("lightgoldenrodyellow", lightgoldenrodyellow)
 , ("lightgray", lightgray)
 , ("lightgreen", lightgreen)
 , ("lightgrey", lightgrey)
 , ("lightpink", lightpink)
 , ("lightsalmon", lightsalmon)
 , ("lightseagreen", lightseagreen)
 , ("lightskyblue", lightskyblue)
 , ("lightslategray", lightslategray)
 , ("lightslategrey", lightslategrey)
 , ("lightsteelblue", lightsteelblue)
 , ("lightyellow", lightyellow)
 , ("lime", lime)
 , ("limegreen", limegreen)
 , ("linen", linen)
 , ("magenta", magenta)
 , ("maroon", maroon)
 , ("mediumaquamarine", mediumaquamarine)
 , ("mediumblue", mediumblue)
 , ("mediumorchid", mediumorchid)
 , ("mediumpurple", mediumpurple)
 , ("mediumseagreen", mediumseagreen)
 , ("mediumslateblue", mediumslateblue)
 , ("mediumspringgreen", mediumspringgreen)
 , ("mediumturquoise", mediumturquoise)
 , ("mediumvioletred", mediumvioletred)
 , ("midnightblue", midnightblue)
 , ("mintcream", mintcream)
 , ("mistyrose", mistyrose)
 , ("moccasin", moccasin)
 , ("navajowhite", navajowhite)
 , ("navy", navy)
 , ("oldlace", oldlace)
 , ("olive", olive)
 , ("olivedrab", olivedrab)
 , ("orange", orange)
 , ("orangered", orangered)
 , ("orchid", orchid)
 , ("palegoldenrod", palegoldenrod)
 , ("palegreen", palegreen)
 , ("paleturquoise", paleturquoise)
 , ("palevioletred", palevioletred)
 , ("papayawhip", papayawhip)
 , ("peachpuff", peachpuff)
 , ("peru", peru)
 , ("pink", pink)
 , ("plum", plum)
 , ("powderblue", powderblue)
 , ("purple", purple)
 , ("red", red)
 , ("rosybrown", rosybrown)
 , ("royalblue", royalblue)
 , ("saddlebrown", saddlebrown)
 , ("salmon", salmon)
 , ("sandybrown", sandybrown)
 , ("seagreen", seagreen)
 , ("seashell", seashell)
 , ("sienna", sienna)
 , ("silver", silver)
 , ("skyblue", skyblue)
 , ("slateblue", slateblue)
 , ("slategray", slategray)
 , ("slategrey", slategrey)
 , ("snow", snow)
 , ("springgreen", springgreen)
 , ("steelblue", steelblue)
 , ("tan", tan)
 , ("teal", teal)
 , ("thistle", thistle)
 , ("tomato", tomato)
 , ("turquoise", turquoise)
 , ("violet", violet)
 , ("wheat", wheat)
 , ("white", white)
 , ("whitesmoke", whitesmoke)
 , ("yellow", yellow)
 , ("yellowgreen", yellowgreen)
 ]

