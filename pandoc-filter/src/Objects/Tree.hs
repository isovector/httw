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

instance (Hashable a, ToDot a, Show a, Read a) => IsBlockObject (Figure (LRose a)) where
  fromBlocks [[Para (Strs fid)], [Para (Strs label)], [Para (Strs obj)]]
    = Right $ Figure fid label $ read $ T.unpack obj
  fromBlocks z = Left $ "Bad format:\n" <> show z
  toBlockObject (Figure fid lbl t) = do
    fp <- doDot t
    pure $ mkPandocFigure fid lbl fp

instance ToDot a => ToDot (LRose a) where
  toDot (LPure a) = toDot a
  toDot (LRose a ros) = do
    ns <- traverse toDot ros
    me <- toDot a
    for_ ns $ addEdge me
    pure me

