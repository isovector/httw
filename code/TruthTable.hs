{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module TruthTable (Enumerable (..), EnumerateEnum(..), truthTable, SplitProduct) where

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing, fromJust)
import GHC.Generics
import Circuitry.Circuit
import Circuitry.Embed
import qualified Clash.Sized.Vector as V


class CustomShow a where
  customShow :: a -> String

instance {-# OVERLAPPING #-} CustomShow Bool where
  customShow True = "On"
  customShow False = "Off"

instance {-# OVERLAPPING #-} CustomShow a => CustomShow (Named nm a) where
  customShow (Named a) = customShow a

instance Show a => CustomShow a where
  customShow = show


class Enumerable a where
  enumerate :: [a]
  default enumerate :: (Generic a, GEnumerable (Rep a)) => [a]
  enumerate = fmap to genumerate

deriving newtype instance Enumerable a => Enumerable (Named nm a)


class GEnumerable f where
  genumerate :: [f x]

instance GEnumerable f => GEnumerable (M1 _1 _2 f) where
  genumerate = fmap M1 genumerate

instance (GEnumerable f, GEnumerable g) => GEnumerable (f :*: g) where
  genumerate = liftA2 (:*:) genumerate genumerate

instance (GEnumerable f, GEnumerable g) => GEnumerable (f :+: g) where
  genumerate = fmap L1 genumerate <> fmap R1 genumerate

instance GEnumerable U1 where
  genumerate = pure U1

instance Enumerable a => GEnumerable (K1 _1 a) where
  genumerate = fmap K1 enumerate


newtype EnumerateEnum a = EnumerateEnum a

instance (Enum a, Bounded a) => Enumerable (EnumerateEnum a) where
  enumerate = coerce $ enumFromTo @a minBound maxBound


deriving anyclass instance Enumerable Bool
deriving anyclass instance (Enumerable a, Enumerable b) => Enumerable (a, b)
deriving anyclass instance (Enumerable a, Enumerable b) => Enumerable (Either a b)
deriving anyclass instance (Enumerable a) => Enumerable (Maybe a)


class SplitProduct a where
  splitProduct :: a -> [String]

instance {-# OVERLAPPING #-} (SplitProduct a, SplitProduct b) => SplitProduct (a, b) where
  splitProduct (a, b) = splitProduct a <> splitProduct b

instance CustomShow a => SplitProduct a where
  splitProduct = pure . customShow


truthTable
    :: forall a b
     . ( Enumerable a
       , Reify a
       , Reify b
       , SplitProduct a
       , SplitProduct b
       )
    => Circuit a b
    -> String
truthTable c = buildTable $ do
  a <- enumerate @a
  pure $ (splitProduct a, ) $
    let v = evalCircuitMV c (fmap Just $ embed a) 0
     in case (all isJust $ V.toList v, all isNothing $ V.toList v) of
          (True, _) -> splitProduct $ reify @b $ fmap fromJust v
          (_, True) -> "high Z" <$ (splitProduct $ reify @b $ V.repeat False)
          (_, _) -> error "circuit for truthTable isn't total"


buildTable :: [([String], [String])] -> String
buildTable ls@((i,o):_)
  = unlines
  $ buildHeader (length i) (length o)
  : buildSpacer (length i + length o)
  : fmap (uncurry buildRow) ls
buildTable [] = error "empty table"


buildHeader :: Int -> Int -> String
buildHeader ins outs
  = columnize
  $ fmap (mappend "In " . show) [1 .. ins] <> fmap (mappend "Out " . show) [1 .. outs]


buildSpacer :: Int -> String
buildSpacer cols
  = columnize
  $ "---" <$ [1 .. cols]


buildRow :: [String] -> [String] -> String
buildRow ins outs = columnize $ ins <> outs


columnize :: [String] -> String
columnize ins = "| " <> intercalate " | " ins <> " |"

