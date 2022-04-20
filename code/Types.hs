module Types where
import GHC.Generics (Generic)

data Beside a
  = Beside [a]
  deriving stock (Eq, Ord, Show, Functor)

data GoesTo a
  = GoesTo String a a
  | Cons String a (GoesTo a)
  deriving stock (Eq, Ord, Show, Functor)


data Rose a = Pure a | Rose [Rose a]
  deriving stock (Eq, Ord, Show, Functor)

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

