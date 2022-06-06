{-# LANGUAGE UndecidableInstances #-}

module Objects.Figure where

import Text.Pandoc.Definition
import GHC.Generics
import Cache (caching, hashFile)
import Data.Text (Text)
import Control.Arrow ((&&&))
import qualified Data.Text as T
import Objects.Internal.Dot
import Data.Foldable (for_)
import System.Process (callProcess)
import Data.Hashable (Hashable)
import Objects.Internal.Types

data Figure a = Figure
  { f_id    :: Text
  , f_label :: Text
  , f_thing :: a
  }
  deriving stock (Eq, Read, Ord, Show, Functor, Generic, Generic1, Foldable, Traversable)

instance Hashable a => Hashable (Figure a) where

-- instance (Hashable a, ToDot a, Show a, Read a) => IsBlockObject (Figure a) where
--   fromBlocks [[Para (Strs fid)], [Para (Strs label)], [Para (Strs obj)]]
--     = Right $ Figure fid label $ read $ T.unpack obj
--   fromBlocks z = Left $ "Bad format:\n" <> show z
--   toBlockObject (Figure fid lbl t) = do
--     fp <- error "figure out how to make me compose"
--     pure $ mkPandocFigure fid lbl fp

mkPandocFigure :: Text -> Text -> FilePath -> Block
mkPandocFigure fid lbl fp =
  Div ("", [], [])
    [ Para [Image (fid, [], []) [Str lbl] (T.pack fp , "fig:")]
    ]


