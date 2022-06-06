{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall     #-}

module Ghci (emitGhci) where

import System.IO
import           Cache
import           Control.Lens
import           Data.Bool
import           Data.Char
import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Process
import           Text.Pandoc (readerExtensions, pandocExtensions, def, readMarkdown)
import           Text.Pandoc.Class (runIOorExplode)
import           Text.Pandoc.JSON



doReplace :: Maybe Text -> String -> String
doReplace = appEndo . maybe mempty parseReplace

parseReplace :: Text -> Endo String
parseReplace x =
  flip foldMap (T.split (== '|') x) $ \t ->
    case T.split (== '=') t of
      [lhs, rhs] -> Endo $ T.unpack . T.replace lhs rhs . T.pack
      _ -> error $ "invalid replace string: " <> T.unpack t

emitGhci :: Block -> IO Block
-- GHCI block
emitGhci (CodeBlock (_, _, kvs) str)
  | Just file <- lookup "ghci" kvs
  = caching (file, str, kvs) $ ghciToPandoc kvs (T.unpack file) (T.unpack str)
-- design block
emitGhci (CodeBlock attr@(_, _, kvs) str)
  | Just file <- lookup "design" kvs
  = caching (file, str, attr, kvs) $ do
      let fn = fromMaybe "__design" $ lookup "fn" kvs
      designHashToPandoc
        (T.unpack fn)
        kvs
        attr
        (T.unpack file) (
        T.unpack str)
emitGhci x = pure x


------------------------------------------------------------------------------
-- | Run a function defined in the module, parsing its output as markdown
designHashToPandoc
    :: String -> [(Text, Text)] -> Attr -> FilePath -> String -> IO Block
designHashToPandoc fn _ attr fp txt = do
  let hash = hashFile (fp, txt, attr)
  let cmds =
        unwords
          [ fn
          , show attr
          , show txt
          , show hash
          , "$\n"
          , unlines $ fmap (mappend "    ") $ lines txt
          ]
  writeFile "/tmp/wtf.hs" cmds
  r <- runGhciVal fp cmds
  hPutStrLn stderr r
  Pandoc _ p
    <- runIOorExplode
      $ readMarkdown def { readerExtensions = pandocExtensions }
      $ T.pack r
  pure $ Div mempty p


ghciToPandoc :: [(Text, Text)] -> FilePath -> String -> IO Block
ghciToPandoc kvs fp = fmap format . runGhci kvs id fp



format :: [(String, Maybe String)] -> Block
format xs
  = CodeBlock ( ""
              , bool ["ghci"]
                     ["haskell", "ghci"]
                  $ not
                  $ containsError
                  $ show xs
              , []
              )
  . T.pack
  . unlines
  . fmap (\case
            (req, Just resp) -> unlines ["> " ++ req, resp]
            (req, Nothing) -> unlines ["> " ++ req]
         )
  $ xs


containsError :: String -> Bool
containsError = isInfixOf "<interactive>"


runGhci :: [(Text, Text)] -> ([String] -> [String]) -> FilePath -> String -> IO [(String, Maybe String)]
runGhci kvs f fp str = do
  hPrint stderr str
  fmap (interleave replace (lines $ replace str) . responses f . replace)
    . readProcess' "stack" ["repl", "--no-load"]
    $ unlines [":l " ++ fp, unlines $ fmap removeSilent $ lines $ str]
 where replace = doReplace $ lookup "replace" kvs

runGhciVal :: FilePath -> String -> IO String
runGhciVal fp str
  = fmap (valResponse $ length $ lines str)
  . readProcess' "stack" ["repl", "--no-load"]
  $ unlines [":l " ++ fp, ":{", str, ":}"]


removeSilent :: String -> String
removeSilent ('@':s) = s
removeSilent s = s


valResponse :: Int -> String -> String
valResponse n
  = unwords
  . drop (n + 3)
  . words
  . head
  . drop 1
  . dropWhile (not . isPrefixOf "Ok, ")
  . lines

responses :: ([String] -> [String]) -> String -> [String]
responses f
  = fmap unlines
  . fmap f
  . fmap (_head %~ removeManyTags)
  . groupBy (\_ a -> not $ isResponse a)
  . drop 1
  . dropWhile (not . isPrefixOf "Ok, ")
  . lines


removeTag :: String -> String
removeTag = drop 2 . dropWhile (/= '>')


removeManyTags :: String -> String
removeManyTags ts = bool ts (removeManyTags $ removeTag ts) $ isResponse ts


isResponse :: String -> Bool
isResponse ('*':_) = True
isResponse x = isPrefixOf "ghci>" x


isSilent :: String -> Bool
isSilent str
  | isPrefixOf ":set "     str = True
  | isPrefixOf "let "      str = True
  | isPrefixOf "type "     str = True
  | isPrefixOf "import "   str = True
  | isPrefixOf "default (" str = True
  | otherwise = isReallySilent str

------------------------------------------------------------------------------
-- | Zip input lines to GHCI with responses from GHCI. Correctly deals with
-- input like @let x = 5@ which doesn't give a response.
interleave :: (String -> String) -> [String] -> [String] -> [(String, Maybe String)]
interleave f as
  = filter (not . null . fst)
  . zipping
      isSilent
      (\a ->
        let a' = f a
         in if isReallySilent a'
               then ("", Nothing)
               else (a', Nothing))
      (\a b -> (f a, Just $ initNonEmpty $ f b))
      (fmap (dropWhile isSpace . f) as)


initNonEmpty :: [a] -> [a]
initNonEmpty [] = []
initNonEmpty a = init a


------------------------------------------------------------------------------
-- | Input that starts wih a @\@@ is considered really silent, and doesn't even
-- appear as prompted input.
isReallySilent :: String -> Bool
isReallySilent str
  | isPrefixOf "@" str = True
  | otherwise = False


------------------------------------------------------------------------------
-- | Probably the worst function I've ever written. I don't know what this
-- does. Sorry!
zipping :: (a -> Bool) -> (a -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipping _ _ _ [] _ = []
zipping _ _ _ _ [] = []
zipping p d f (a:as) bs | p a = d a   : zipping p d f as bs
zipping p d f (a:as) (b:bs)   = f a b : zipping p d f as bs

readProcess' :: String -> [String] -> String -> IO String
readProcess' cmd args = readCreateProcess $ shell $ intercalate " " (cmd : args) <> " 2>&1"

