module Objects.Internal.Types where

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


class FromBlocks a where
  fromBlocks :: [[Block]] -> Either String a

class Show a => IsBlockObject a where
  toBlockObject :: a -> IO Block


------------------------------------------------------------------------------

pattern Strs :: Text -> [Inline]
pattern Strs ts <-
  ((id &&& id)
    ->
      ( all isStr -> True
      , foldMap fromStr -> ts
      )
  )

isStr :: Inline -> Bool
isStr (Str _) = True
isStr Space = True
isStr (Quoted _ x) = all isStr x
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr (Quoted SingleQuote s) = "'" <> foldMap fromStr s <> "'"
fromStr (Quoted DoubleQuote s) = "\"" <> foldMap fromStr s <> "\""
fromStr Space = " "
fromStr _ = error "fromStr called on not a str"


