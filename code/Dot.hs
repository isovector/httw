module Dot where

import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.List (group)
import Data.Char (isAlphaNum)
import System.Process (callProcess)
import Data.Foldable (for_)


newtype DotM a = DotM
  { unDotM :: StateT Int (Writer [String]) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    , MonadWriter [String]
    )

instance Show (DotM a) where
  show _ = "A tree"


data Rule a = Rule a a
  deriving stock (Eq, Ord, Show, Functor)

instance ToDot a => ToDot (Rule a) where
  toDot (Rule l r) = do
    ln <- cluster "Lhs" $ toDot l
    larr <- invisNode
    rarr <- invisNode
    addEdge larr rarr
    rn <- cluster "Rhs" $ toDot r
    sameRank [ln, larr, rarr, rn]
    pure rn


data Rose a = Pure a | Rose [Rose a]
  deriving stock (Eq, Ord, Show, Functor)

instance Show a => ToDot (Rose a) where
  toDot (Pure a) = newNode $ show a
  toDot (Rose ros) = do
    ns <- traverse toDot ros
    me <- newNode "&otimes;"
    for_ ns $ addEdge me
    pure me



data Metavar = Club | Diamond | Spade | Heart
  deriving stock (Eq, Ord)

instance Show Metavar where
  show Club = "&clubs;"
  show Diamond = "&diams;"
  show Spade = "&spades;"
  show Heart = "&hearts;"

preamble :: [String]
preamble =
  [ "digraph g {"
  , "bgcolor=\"transparent\";"
  , "compound=true;"
  , "newrank=true;"
  ]

runDotM :: DotM a -> String
runDotM
  = unlines
  . (join preamble :)
  . (++ ["}"])
  . snd
  . runWriter
  . flip evalStateT 0
  . unDotM


class ToDot a where
  toDot :: a -> DotM Node

instance ToDot (DotM Node) where
  toDot = id


newtype Node = Node
  { unNode :: Int
  }
  deriving stock (Eq, Ord, Show)


nodeName :: Node -> String
nodeName = ('n' :) . show . unNode


newNode :: String -> DotM Node
newNode label = do
  n <- fmap Node get
  modify' (+ 1)
  tell $ pure $ nodeName n <> "[label=" <> show label <> "]"
  pure n


invisNode :: DotM Node
invisNode = do
  n <- fmap Node get
  modify' (+ 1)
  tell $ pure $ nodeName n <> "[style=\"invis\"]"
  pure n


sameRank :: [Node] -> DotM ()
sameRank ns = do
  tell $ pure "{ rank=same; "
  for_ ns $ tell . pure . (<> ";") . nodeName
  tell $ pure "}"


cluster :: String -> DotM a -> DotM a
cluster label m = do
  tell $ pure $ "subgraph cluster" <> label <> " {"
  r <- m
  tell $ pure "}"
  pure r


addEdge :: Node -> Node -> DotM ()
addEdge n1 n2 = do
  tell $ pure $ nodeName n1 <> " -> " <> nodeName n2


__design
    :: (ToDot a, Show a)
    => (String, [String], [(String, String)])
    -> String
    -> String
    -> a
    -> IO ()
__design (name, _, kvs) _ hash a = do
  let fp = hash <> ".png"
      label = fromMaybe (show a) $ lookup "label" kvs
      figname = bool name ("fig:" <> __makeFigName (show label))  $ name == ""

  writeFile "/tmp/output.dot" $ runDotM $ toDot a

  callProcess "dot"
    [ "-Tpng"
    , "-Gdpi=300"
    , "-o/tmp/out.png"
    , "/tmp/output.dot"
    ]
  callProcess "convert"
    [ "/tmp/out.png"
    , "-density"
    , "300"
    , "-units"
    , "pixelsperinch"
    , "-resize"
    , "55%"
    , fp
    ]

  putStrLn $
    mconcat ["![", label, "](", fp, "){#", figname , "}"]


__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'

