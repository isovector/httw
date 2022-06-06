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

instance (FromBlocks a) => FromBlocks (Figure a) where
  fromBlocks ([Para (Strs fid)] : [Para (Strs label)] : bs)
    = Figure fid label <$> fromBlocks bs
  fromBlocks z = Left $ "Bad format (Figure):\n" <> show z

class IsImage a where
  writeImage :: a -> IO FilePath

instance (Show a, IsImage a) => IsBlockObject (Figure a) where
  toBlockObject (Figure fid lbl a) = do
    fp <- writeImage a
    pure $ mkPandocFigure fid lbl fp

mkPandocFigure :: Text -> Text -> FilePath -> Block
mkPandocFigure fid lbl fp =
  Div ("", [], [])
    [ Para [Image (fid, [], []) [Str lbl] (T.pack fp , "fig:")]
    ]


