{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Design where

import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Data.List (group)
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding ((.), id, sum)
import System.Process (callProcess)
import Circuitry.Machinery
import Take2.Computer.Simple
import Take2.Computer.Math
import Circuitry.Instances
import TruthTable


__design
    :: (SeparatePorts a, SeparatePorts b, Embed a, Embed b)
    => (String, [String], [(String, String)])
    -> String
    -> String
    -> Circuit a b
    -> IO ()
__design (name, _, kvs) txt hash c = do
  let fp = hash <> ".png"
      label = fromMaybe txt $ lookup "label" kvs
      figname = bool name ("fig:" <> __makeFigName label)  $ name == ""
      expand_gates = isJust $ lookup "gates" kvs
      depth = maybe 0 read $ lookup "depth" kvs

  writeFile "/tmp/output.json"
    $ renderModuleString
    $ getGraph (RenderOptions expand_gates False depth) c
  callProcess "netlistsvg"
    [ "/tmp/output.json"
    , "-o"
    , "/tmp/output.svg"
    , "--skin"
    , "/home/sandy/prj/circuitry/skin.svg"
    ]
  callProcess "inkscape"
    [ "/tmp/output.svg"
    , "-d"
    , "300"
    , "-o"
    , fp
    ]

  putStrLn $
    mconcat ["![", label, "](", fp, "){#", figname , "}"]


truth
    :: ( Enumerable a
       , Embed a
       , Embed b
       , SplitProduct a
       , SplitProduct b
       )
    => (String, [String], [(String, String)])
    -> String
    -> String
    -> Circuit a b
    -> IO ()
truth _ _ _ c =
  putStrLn $
    truthTable c


__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'

