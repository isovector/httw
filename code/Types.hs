module Types
  ( module Types
  , Colour
  , AlphaColour
  , withOpacity
  , rgb
  ) where

import           Data.Bool (bool)
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL (hsl)
import           Data.Colour.RGBSpace.HSV (hsv, hsvView, value)
import           Data.Colour.SRGB.Linear (rgb, toRGB)
import           Data.Function (on)
import           Data.List (sortOn)
import           Data.List.NonEmpty hiding (head)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Typeable
import           Diagrams.Backend.Rasterific (B)
import           Diagrams.Prelude hiding (Empty, value)
import           GHC.Generics


sHSV :: (Floating b, RealFrac b) => b -> b -> b -> Colour b
sHSV h s v = uncurryRGB rgb $ hsv h s v

sHSL :: (Floating b, RealFrac b) => b -> b -> b -> Colour b
sHSL h s v = uncurryRGB rgb $ hsl h s v

data Ctor = Ctor String | FakeCtor String
  deriving stock (Eq, Ord, Show)

data Beside a
  = Beside [a]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Focused a
  = Focused a
  | Unfocused a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data GoesTo a b
  = GoesTo String a b
  | Cons String a (GoesTo a b)
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

data Bin a = L a | Br (Bin a) (Bin a)
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

data Search a = Empty | Split a (Search a) (Search a)
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

focus :: (Eq a, Functor f) => a -> f a -> f (Focused a)
focus a = fmap $ \x -> bool Unfocused Focused (x == a) x

leaf :: a -> Search a
leaf a = Split a Empty Empty

data Rose a = Pure a | Rose [Rose a]
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

data LRose a = LPure a | LRose a [LRose a]
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

data Metavar = Club | Diamond | Spade | Heart
  deriving stock (Eq, Ord, Generic)

instance Show Metavar where
  show Club = "&clubs;"
  show Diamond = "&diams;"
  show Spade = "&spades;"
  show Heart = "&hearts;"

data Var a = Var | Txt String | Lit a
  deriving stock (Eq, Ord, Show, Functor)

data Fn a = Fn String [Var a]
  deriving stock (Eq, Ord, Show, Functor)

data Schema
  = SPlus [Schema]
  | STimes Ctor [Either String Metavar]
  | SList (NonEmpty (Either String Metavar))
  deriving stock (Eq, Ord, Show)

newtype ColorName = ColorName
  { unColorName :: Colour Double
  }
  deriving stock (Eq, Show, Typeable)

instance Ord ColorName where
  compare = on compare show

instance IsName ColorName

newtype ColorSet = ColorSet
  { getColorSet :: Set ColorName
  }
  deriving stock (Eq, Ord, Show)

colorSet :: Diagram B -> ColorSet
colorSet
  = ColorSet
  . S.fromList
  . (toListOf eachName =<<)
  . fmap fst
  . names


showOne :: (Colour Double -> Colour Double -> AlphaColour Double) -> Trail V2 Double -> (Colour Double -> Colour Double) -> Colour Double -> Diagram B -> Diagram B
showOne ac x f i = showSome ac x f [i]

connect''
    :: (Colour Double -> Colour Double -> AlphaColour Double)
    -> Trail V2 Double
    -> Colour Double
    -> Name
    -> Colour Double
    -> Name
    -> Diagram B
    -> Diagram B
connect'' ac x col1 n1 col2 n2 d
  | n1 == n2 = d
  | otherwise =
      let acol = ac col1 col2
       in
        connect'
          (def
            & shaftStyle %~ lcA acol . lw veryThick
            & headStyle %~ lcA acol . fcA acol
            & arrowShaft .~ x
          ) n1 n2 d


curvyArrow :: (TrailLike t, V t ~ V2) => t
curvyArrow = cubicSpline False (fmap p2 [(0, 0), (1, 0.4), (2, 0)])

contrastify :: Double -> Colour Double -> Colour Double
contrastify a c =
  case value (toSRGB c) < 0.51 of
    True -> saturate 0.5 $ blend (1 - a) c white
    False -> darken a c

showSome :: (Colour Double -> Colour Double -> AlphaColour Double) -> Trail V2 Double -> (Colour Double -> Colour Double) -> [Colour Double] -> Diagram B -> Diagram B
showSome ac x f is d =
  let cs = colorSet d
   in foldr (\i -> connect'' ac x i (nearestColor cs i) (f i) (nearestColor cs (f i))) d is

showAll :: (Colour Double -> Colour Double -> AlphaColour Double) -> Trail V2 Double -> (Colour Double -> Colour Double) -> Diagram B -> Diagram B
showAll ac x f d =
  let cs = colorSet d
   in foldr (\cn@(ColorName c) -> connect'' ac x c (toName cn) (f c) $ nearestColor cs $ f c) d
    $ S.toList
    $ getColorSet cs

saturate :: RealFloat a => a -> Colour a -> Colour a
saturate a (hsvView . toRGB -> (h, s, v)) = sHSV h (s + a) v

nearestColor :: ColorSet -> Colour Double -> Name
nearestColor (ColorSet s) (toRGB -> RGB r g b)
  = toName
  $ head
  $ sortOn
      ( (\(RGB r1 g1 b1) -> (r - r1) ** 2 + (g - g1) ** 2 + (b - b1) ** 2)
        . toRGB
        . unColorName
      )
  $ S.toList s

contrastingArrow :: Colour Double -> Colour Double -> AlphaColour Double
contrastingArrow c _ = opaque $ contrastify 0.5 c


