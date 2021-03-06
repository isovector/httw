{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib where

import           Combinators
import           Control.Arrow
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable
import           Data.List
import           Data.List (uncons, sort)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Snippets
import           Text.Pandoc



headerClassAppend :: Format -> Text -> Text -> Block -> Block
headerClassAppend format cls color (Header n (ident, attrs, kvs) contents)
  | elem cls attrs
  = Header n (ident, attrs, kvs)
  $ RawInline (Format "latex") ("{\\color{" <> color <> "!100!black}")
  : contents <> [RawInline (Format "latex") "}"]
headerClassAppend _ _ _ x = x


wrapCodeEnv :: Format -> Text -> Text -> Maybe Text -> Block -> Block
wrapCodeEnv format cls div (Just arg) (CodeBlock (ident, attrs, kvs) code)
  | Just v <- lookup arg kvs
  , elem cls attrs
      = mkEnv format div [v]
      $ pure
      $ CodeBlock (ident, attrs, kvs) code
wrapCodeEnv _ _ _ _ t = t


inlineSnippets :: Block -> IO Block
inlineSnippets = \case
  Para [Link (_, _, kvs) (Strs t) ("Snip", _)]  -> runSnippet t kvs
  Plain [Link (_, _, kvs) (Strs t) ("Snip", _)] -> runSnippet t kvs
  t -> pure t

  where
    runSnippet :: Text -> [(Text, Text)] -> IO Block
    runSnippet args kvs = do
      (fp : more_args) <- pure $ splitArgs args
      snippet (T.unpack fp) kvs $ fmap fst $ uncons more_args

    snippet :: FilePath -> [(Text, Text)] -> Maybe Text -> IO Block
    snippet fp kvs defn = do
      file <- readFile fp
      pure
        $ codeBlock
        $ flip (foldr (uncurry T.replace)) kvs
        $ T.pack
        $ getDefinition fp file
        $ fmap T.unpack defn



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
isStr _ = False

fromStr :: Inline -> Text
fromStr (Str s) = s
fromStr Space = " "


showVector :: V.Vector Text -> Block
showVector = BulletList
           . fmap (pure . Plain . pure . Str)
           . sort
           . toList


codeBlock :: Text -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: Text -> [Text]
splitArgs s =
  case T.break (== ':') s of
    ("", "") -> []
    (as, "") -> [as]
    (as, T.drop 1 -> bs) -> as : splitArgs bs

