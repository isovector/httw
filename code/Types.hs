module Types where

import GHC.Generics
import Data.List.NonEmpty
import Data.Bool (bool)

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
  | STimes String [Either String Metavar]
  | SList (NonEmpty (Either String Metavar))
  deriving stock (Eq, Ord, Show)

