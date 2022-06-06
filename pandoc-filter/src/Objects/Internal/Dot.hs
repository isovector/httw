module Objects.Internal.Dot where

import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.List (group)
import Data.Char (isAlphaNum)
import System.Process (callProcess)
import Data.Foldable (for_, foldrM, toList)
import Data.List.NonEmpty (NonEmpty (..))


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

instance {-# OVERLAPPING #-} ToDot String where
  toDot = newNode

instance ToDot Int where
  toDot = newNode . show

instance ToDot Bool where
  toDot = newNode . show

instance ToDot Float where
  toDot = newNode . show

instance ToDot Double where
  toDot = newNode . show

instance ToDot a => ToDot [a] where
  toDot [] = ctorNode "Nil"
  toDot (a : as) = do
    n <- toDot as
    me <- toDot a
    addEdge me n
    pure me

instance ToDot a => ToDot (NonEmpty a) where
  toDot = go . toList
    where
      go [] = error "impossible"
      go [a] = toDot a
      go (a : as) = do
        n <- go as
        me <- toDot a
        addEdge me n
        pure me


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
newNode "" = shapedNode "point" ""
newNode label = shapedNode "oval" label

ctorNode :: String -> DotM Node
ctorNode = shapedNode "invhouse"


invisNode :: DotM Node
invisNode = do
  n <- fmap Node get
  modify' (+ 1)
  tell $ pure $ nodeName n <> "[style=\"invis\";width=0.01]"
  pure n

shapedNode :: String -> String -> DotM Node
shapedNode shape label = do
  n <- fmap Node get
  modify' (+ 1)
  tell $ pure $ nodeName n <> "[shape=" <> show shape <> ";label=" <> show label <> "]"
  pure n


sameRank :: [Node] -> DotM ()
sameRank ns = do
  tell $ pure "{ rank=same; "
  for_ ns $ tell . pure . (<> ";") . nodeName
  tell $ pure "}"


cluster :: Maybe String -> DotM a -> DotM a
cluster label m = do
  name <- fmap (show . (+ (length $ show label))) fresh
  tell $ pure $ "subgraph cluster" <> name <> " {"
  tell $ pure $ "label = " <> maybe (show "") show label <> ";"
  r <- m
  tell $ pure "}"
  pure r


addEdge :: Node -> Node -> DotM ()
addEdge n1 n2 = do
  tell $ pure $ nodeName n1 <> " -> " <> nodeName n2


addLabeledEdge :: String -> Node -> Node -> DotM ()
addLabeledEdge lbl n1 n2 = do
  tell $ pure $ nodeName n1 <> " -> " <> nodeName n2 <> "[label=" <> show lbl <> "]"



__makeFigName :: String -> String
__makeFigName
    = concatMap (\x -> bool x (take 1 x) $ take 1 x == "_")
    . group
    . fmap go
  where
    go c | isAlphaNum c = c
    go _ = '_'

instance ToDot Char where
  toDot = newNode . pure

makeTree :: ToDot a => String -> [a] -> DotM Node
makeTree s as = do
  n <- newNode s
  ns <- traverse toDot as
  foldrM (\a n' -> addEdge n' a >> pure n') n ns

makeLabeledTree :: ToDot a => String -> [(String, a)] -> DotM Node
makeLabeledTree s (unzip -> (lbs, as)) = do
  n <- newNode s
  ns <- traverse toDot as
  foldrM (\(lbl, a) n' -> addLabeledEdge lbl n' a >> pure n') n $ zip lbs ns



