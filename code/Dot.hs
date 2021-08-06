module Dot where

import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.List (group)
import Data.Char (isAlphaNum)
import System.Process (callProcess)
import Data.Foldable (for_, foldrM)


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

data Beside a
  = Beside [a]
  deriving stock (Eq, Ord, Show, Functor)

instance ToDot a => ToDot (Beside a) where
  toDot (Beside ls) = do
    names <- traverse (const $ fmap show fresh) ls
    foldr1 f
      $ fmap (uncurry cluster)
      $ zip names
      $ fmap toDot ls
    where
      f l r = do
        ln <- toDot l
        rn <- toDot r
        sameRank [ln, rn]
        pure rn


data GoesTo a
  = GoesTo String a a
  | Cons String a (GoesTo a)
  deriving stock (Eq, Ord, Show, Functor)

instance ToDot a => ToDot (GoesTo a) where
  toDot (GoesTo lbl l r) = do
    lname <- fmap show fresh
    rname <- fmap show fresh
    ln <- cluster lname $ toDot l
    larr <- invisNode
    rarr <- invisNode
    addLabeledEdge lbl larr rarr
    rn <- cluster rname $ toDot r
    sameRank [ln, larr, rarr, rn]
    pure rn
  toDot (Cons lbl l r) = do
    lname <- fmap show fresh
    ln <- cluster lname $ toDot l
    larr <- invisNode
    rarr <- invisNode
    addLabeledEdge lbl larr rarr
    rn <- toDot r
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


fresh :: DotM Int
fresh = get <* modify' (+ 1)

newNode :: String -> DotM Node
newNode label = do
  n <- fmap Node fresh
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


addLabeledEdge :: String -> Node -> Node -> DotM ()
addLabeledEdge lbl n1 n2 = do
  tell $ pure $ nodeName n1 <> " -> " <> nodeName n2 <> "[label=" <> show lbl <> "]"


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
    , "40%"
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


makeTree :: ToDot a => String -> [a] -> DotM Node
makeTree s as = do
  n <- newNode s
  ns <- traverse toDot as
  foldrM (\a n' -> addEdge n' a >> pure n') n ns



