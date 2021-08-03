module Languages.And where

import Dot

data AndLang = Y | N | A AndLang AndLang | MV Metavar
  deriving stock (Eq, Ord)

instance Show AndLang where
  showsPrec _ Y = showString "Y"
  showsPrec _ N = showString "N"
  showsPrec _ (MV m) = showString $ show m
  showsPrec p (A l r) = showParen (p /= 0) $ showsPrec 10 l . showString "A" . showsPrec 10 r

instance ToDot AndLang where
  toDot Y = newNode "Y"
  toDot N = newNode "N"
  toDot (MV m) = newNode $ show m
  toDot (A l r) = do
    ln <- toDot l
    a <- newNode "A"
    rn <- toDot r
    addEdge a ln
    addEdge a rn
    pure a

