{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphDot where

import Languages.Information
import Diagrams.TwoD.GraphViz hiding (mkGraph)
import Data.GraphViz.Attributes.Complete (GraphvizCommand(Dot))
import GHC.Generics
import Diagrams.Prelude hiding (from, p2)
import Diagrams.Backend.Rasterific
import Control.Monad.State
import Control.Monad.Writer
import Diagram
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Either (partitionEithers)
import System.IO.Unsafe (unsafePerformIO)
import Data.Function

type Node = Int

data CmpFst a b = CmpFst
  { cf_fst :: a
  , cf_snd :: b
  }

instance Eq a => Eq (CmpFst a b) where
  (==) = on (==) cf_fst

instance Ord a => Ord (CmpFst a b) where
  compare = on compare cf_fst

data Edge = Edge
  { e_src :: Node
  , e_dst :: Node
  , e_label :: String
  }

toFglEdge :: Edge -> (Node, Node, String)
toFglEdge (Edge n i s) = (n, i, s)

class Diagramize f where
  diagramize :: f x -> StateT Node (Writer [Either (Node, Diagram B) Edge]) Node

instance Diagramize f => Diagramize (M1 _1 _2 f) where
  diagramize (M1 f) = diagramize f

instance Diagramize U1 where
  diagramize U1 = do
    n <- newNode
    tell $ pure $ Left $ (n, ) $ rect 1 1 # center # fc black
    pure n

emitNode :: MonadWriter [Either (Node, Diagram B) Edge] m => Diagram B -> StateT Node m Node
emitNode d = do
  n <- newNode
  tell . pure . Left $ (n ,) d
  pure n

emitEdge :: MonadWriter [Either (Node, Diagram B) Edge] m => Edge -> m ()
emitEdge = tell . pure . Right

newNode :: Monad m => StateT Node m Node
newNode = get <* modify (+1)

instance (Diagramize f, Diagramize g) => Diagramize (f :*: g) where
  diagramize (f :*: g) = do
    fn <- diagramize f
    gn <- diagramize g
    n <- emitNode $ rect 10 10 # center # fc blue
    emitEdge $ Edge n fn ""
    emitEdge $ Edge n gn ""
    pure n

instance (Diagramize f, Diagramize g) => Diagramize (f :+: g) where
  diagramize (L1 f) = do
    fn <- diagramize f
    n <- emitNode $ rect 5 10 # center # fc red
    emitEdge $ Edge n fn ""
    pure n
  diagramize (R1 f) = do
    fn <- diagramize f
    n <- emitNode $ rect 10 5 # center # fc red
    emitEdge $ Edge n fn ""
    pure n

instance ToDiagram a => Diagramize (K1 _1 a) where
  diagramize = emitNode . sized (dims 10) . toDiagram . unK1


newtype ViaDiagramize a = ViaDiagramize { unViaDiagramize :: a }

mkNode :: (Node, a) -> (Node, CmpFst Node a)
mkNode (n, a) = (n, CmpFst n a)

instance (Generic a, Diagramize (Rep a)) => ToDiagram (ViaDiagramize a) where
  toDiagram a = unsafePerformIO $ do
    let (ns, es) = partitionEithers
                 $ snd
                 $ runWriter
                 $ flip runStateT 0
                 $ diagramize
                 $ from
                 $ unViaDiagramize a
        g = mkGraph (fmap mkNode ns) $ fmap toFglEdge es
        opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)

    g' <- layoutGraph Dot g
    pure $ frame 10 $
      drawGraph
        (\d -> place (cf_snd d))
        (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)
        g'
  {-# NOINLINE toDiagram #-}

deriving via ViaDiagramize (Maybe a) instance ToDiagram a => ToDiagram (Maybe a)
deriving via ViaDiagramize (a, b) instance (ToDiagram a, ToDiagram b) => ToDiagram (a, b)


