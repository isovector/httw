{-# LANGUAGE UndecidableInstances #-}
module Objects.Internal.Types where

import           Control.Arrow ((&&&))
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Pandoc.Definition
import           Text.Read (readMaybe)


class FromBlocks a where
  fromBlocks :: [[Block]] -> Either String a

class Show a => IsBlockObject a where
  toBlockObject :: a -> IO Block


newtype Readable a = Readable a

instance Read a => FromBlocks (Readable a) where
  fromBlocks [[Para (Strs obj)]] =
    case readMaybe $ T.unpack obj of
      Nothing -> Left $ "Bad read parse:\n" <> show obj
      Just x -> Right $ Readable x
  fromBlocks z = Left $ "Bad format:\n" <> show z

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
fromStr SoftBreak = " "
fromStr _ = error "fromStr called on not a str"

fromBlockStr :: Block -> Text
fromBlockStr (Para xs) = foldMap fromStr xs
fromBlockStr (Plain xs) = foldMap fromStr xs
fromBlockStr _ = error "not a block string"


