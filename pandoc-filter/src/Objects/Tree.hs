{-# LANGUAGE UndecidableInstances #-}

module Objects.Tree where

import Text.Pandoc.Definition
import GHC.Generics
import Data.Text (Text)
import Control.Arrow ((&&&))
import qualified Data.Text as T
import Objects.Internal.Dot
import Data.Foldable (for_)
import System.Process (callProcess)
import Data.Hashable (Hashable)
import Objects.Internal.Types
import Objects.Figure
import Cache (hashFile)


data LRose a = LPure a | LRose a [LRose a]
  deriving stock (Eq, Read, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

instance Hashable a => Hashable (LRose a) where

instance Read a => FromBlocks (LRose a) where
  fromBlocks [[Para (Strs obj)]] = Right $ read $ T.unpack obj
  fromBlocks z = Left $ "Bad format:\n" <> show z

instance (Hashable a, ToDot a) => IsImage (LRose a) where
  writeImage = doDot

instance ToDot a => ToDot (LRose a) where
  toDot (LPure a) = toDot a
  toDot (LRose a ros) = do
    ns <- traverse toDot ros
    me <- toDot a
    for_ ns $ addEdge me
    pure me

