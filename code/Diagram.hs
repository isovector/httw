{-# LANGUAGE OverloadedStrings #-}

module Diagram where

import Diagrams.Prelude hiding (trim)
import Diagrams.Backend.Rasterific
import Data.Char (isAlphaNum)
import Data.Bool (bool)
import Data.List (group, dropWhileEnd)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T


__design
    :: ToDiagram a
    => (String, [String], [(String, String)])
    -> String
    -> String
    -> a
    -> IO ()
__design (name, _, kvs) txt hash c = do
  let fp = hash <> ".png"
      label = fromMaybe txt $ lookup "label" kvs
      figname = bool name ("fig:" <> __makeFigName label) $ name == ""

  renderRasterific fp (dims 150) $ toDiagram c

  putStrLn $
    mconcat ["![", T.unpack (T.replace "\n" " " $ T.pack label), "](", fp, "){#", figname , "}"]

trim :: String -> String
trim = dropWhile (== ' ') . dropWhileEnd (== ' ')

class ToDiagram a where
  toDiagram :: a -> Diagram B

instance ToDiagram (QDiagram B V2 Double Any) where
  toDiagram = id


__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'


