module Diagram where

import Diagrams.Prelude
import Diagrams.Backend.Rasterific
import Data.Char (isAlphaNum)
import Data.Bool (bool)
import Data.List (group)
import Data.Maybe (fromMaybe)


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
      figname = bool name ("fig:" <> __makeFigName label)  $ name == ""

  renderRasterific fp (dims 600) $ toDiagram c

  putStrLn $
    mconcat ["![", label, "](", fp, "){#", figname , "}"]

class ToDiagram a where
  toDiagram :: a -> Diagram B


__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'


