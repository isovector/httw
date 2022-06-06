{-# LANGUAGE UndecidableInstances #-}

module Objects.Tree where

import GHC.Generics
import Objects.Internal.Dot
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Objects.Internal.Types
import Objects.Figure
import Objects.Internal.Parser
import Text.Megaparsec
import Data.Bifunctor (first)


data LRose a = LPure a | LRose a [LRose a]
  deriving stock (Eq, Read, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

instance FromBlocks (LRose String) where
  fromBlocks [blss]
    = first errorBundlePretty
    $ parse (parseLRose <* eof) ""
    $ foldMap fromBlockStr blss
  fromBlocks z = Left $ "Bad format\n" <> show z

instance Hashable a => Hashable (LRose a) where

instance (Hashable a, ToDot a) => IsImage (LRose a) where
  writeImage = doDot

instance ToDot a => ToDot (LRose a) where
  toDot (LPure a) = toDot a
  toDot (LRose a ros) = do
    ns <- traverse toDot ros
    me <- toDot a
    for_ ns $ addEdge me
    pure me


parseLRose :: Parser (LRose String)
parseLRose = optionalSurround (sym "(") (sym ")") $ do
  node <- str
  children
    <- optional
     $ surrounded (sym "[") (sym "]")
     $ sepBy parseLRose
     $ sym ","
  pure $ case children of
    Nothing -> LPure node
    Just lrs -> LRose node lrs

