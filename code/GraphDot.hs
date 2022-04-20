{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GraphDot where

import Diagrams.TwoD.GraphViz hiding (mkGraph)
import Data.GraphViz.Attributes.Complete (GraphvizCommand(Dot))
import GHC.Generics
import Diagrams.Prelude hiding (local, Color, from, p2)
import Diagrams.Backend.Rasterific
import Control.Monad.State
import Control.Monad.Writer
import Diagram
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Either (partitionEithers)
import System.IO.Unsafe (unsafePerformIO)
import Data.Function
import Data.Foldable (for_)
import GHC.TypeLits
import Data.Proxy
import Data.Bifunctor (first)
import Types
import Control.Monad.RWS
import Debug.Trace (traceShowId)

type Node = Int

data CmpFst a b = CmpFst
  { cf_fst :: a
  , cf_snd :: b
  }

instance ToDiagram Metavar where
  toDiagram Club    = mkText "C" -- "♣"
  toDiagram Diamond = mkText "D" -- "♦"
  toDiagram Spade   = mkText "S" -- "♠"
  toDiagram Heart   = mkText "H" -- "♥"

instance Eq a => Eq (CmpFst a b) where
  (==) = on (==) cf_fst

instance Ord a => Ord (CmpFst a b) where
  compare = on compare cf_fst

instance Show a => Show (CmpFst a b) where
  show = show . cf_fst

data Edge = Edge
  { e_src :: Node
  , e_dst :: Node
  , e_label :: String
  }

mkText :: String -> Diagram B
mkText = scale 15 . center . texterific

toFglEdge :: Edge -> (Node, Node, String)
toFglEdge (Edge n i s) = (n, i, s)


class Diagramize a where
  diagramize :: a -> RWS (Diagram B -> Diagram B) [Either (Node, Diagram B) Edge] Node [(String, Node)]

class GDiagramize f where
  gdiagramize :: f x -> RWS (Diagram B -> Diagram B) [Either (Node, Diagram B) Edge] Node [(String, Node)]

instance GDiagramize f => GDiagramize (D1 _1 f) where
  gdiagramize (M1 f) = gdiagramize f

instance (KnownSymbol name, GDiagramize f) => GDiagramize (S1 ('MetaSel ('Just name) _1 _2 _3) f) where
  gdiagramize (M1 f) = fmap (fmap $ first $ const $ symbolVal $ Proxy @name) $ gdiagramize f

instance (GDiagramize f) => GDiagramize (S1 ('MetaSel 'Nothing _1 _2 _3) f) where
  gdiagramize (M1 f) = gdiagramize f

instance (KnownSymbol name, GDiagramize f) => GDiagramize (C1 ('MetaCons name _2 _3) f) where
  gdiagramize (M1 f) = do
    ns <- gdiagramize f
    r@(_, n) <- emitNode $ mkText $ nameof @name
      -- circle 3 & center & fc black
    for_ ns $ \(e, n') -> emitEdge $ Edge n n' e
    pure $ pure r

nameof :: forall name. KnownSymbol name => String
nameof =
  case symbolVal $ Proxy @name of
    ":" -> "Cons"
    "[]" -> "Nil"
    x -> x


instance GDiagramize U1 where
  gdiagramize U1 = pure []
    -- fmap pure $ emitNode $ circle 3 # center # fc black

emitNode
    :: ( MonadState Node m
       , MonadWriter [Either (Node, Diagram B) Edge] m
       , MonadReader (Diagram B -> Diagram B) m
       )
    => Diagram B -> m (String, Node)
emitNode d = do
  n <- newNode
  f <- ask
  tell . pure . Left $ (n ,) $ f d
  pure ("", n)

emitEdge :: MonadWriter [Either (Node, Diagram B) Edge] m => Edge -> m ()
emitEdge = tell . pure . Right

newNode :: MonadState Node m => m Node
newNode = get <* modify (+1)

instance (GDiagramize f, GDiagramize g) => GDiagramize (f :*: g) where
  gdiagramize (f :*: g) = do
    fn <- gdiagramize f
    gn <- gdiagramize g
    -- n <- emitNode $ circle 3 # center # fc black
    -- emitEdge $ Edge n fn ""
    -- emitEdge $ Edge n gn ""
    pure $ fn <> gn

instance (GDiagramize f, GDiagramize g) => GDiagramize (f :+: g) where
  gdiagramize (L1 f) = do
    fn <- gdiagramize f
    -- n <- emitNode $ rect 5 10 # center # fc red
    -- emitEdge $ Edge n fn ""
    pure fn
  gdiagramize (R1 f) = do
    fn <- gdiagramize f
    -- n <- emitNode $ rect 10 5 # center # fc red
    -- emitEdge $ Edge n fn ""
    pure fn

instance Diagramize a => GDiagramize (K1 _1 a) where
  gdiagramize = diagramize . unK1


newtype ViaDiagramize a = ViaDiagramize { unViaDiagramize :: a }
newtype ViaToDiagram a = ViaToDiagram { unViaToDiagram :: a }

instance ToDiagram a => Diagramize (ViaToDiagram a) where
  diagramize = fmap pure . emitNode . toDiagram . unViaToDiagram

mkNode :: (Node, a) -> (Node, CmpFst Node a)
mkNode (n, a) = (n, CmpFst n a)

instance (Generic a, GDiagramize (Rep a)) => Diagramize (ViaDiagramize a) where
  diagramize = gdiagramize . from . unViaDiagramize

instance {-# OVERLAPPABLE #-} (Diagramize a) => ToDiagram a where
  toDiagram a = unsafePerformIO $ do
    let (ns, es) = partitionEithers
                 $ (\ (_, _, c) -> c)
                 $ runRWS (diagramize a) id 0
        g = mkGraph (fmap mkNode ns) $ fmap toFglEdge es
        opts p _ = with
          & gaps .~ 16
          & arrowShaft .~ (unLoc . head $ pathTrails p)

    g' <- layoutGraph Dot g
    pure $ center $ frame 10 $
      drawGraph
        (\d -> place (cf_snd d))
        (\_ p1 _ p2 e p -> arrowBetween' (opts p e) p1 p2)
        g'
  {-# NOINLINE toDiagram #-}

deriving via ViaDiagramize (Maybe a) instance Diagramize a => Diagramize (Maybe a)
deriving via ViaDiagramize (a, b) instance (Diagramize a, Diagramize b) => Diagramize (a, b)
deriving via ViaDiagramize () instance Diagramize ()
deriving via ViaToDiagram Metavar instance Diagramize Metavar

instance (ToDiagram a, ToDiagram b) => ToDiagram (Either a b) where
  toDiagram (Left a) =
    mkText "Left " ||| toDiagram a
  toDiagram (Right b) =
    mkText "Right " ||| toDiagram b



instance ToDiagram a => Diagramize [a] where
  diagramize [] = fmap pure $ emitNode $ center $ bgFrame 10 grey $ mkText "Done"
  diagramize (a : as) = do
    ns <- diagramize as
    z@(_, n) <- emitNode $ center $ bgFrame 10 grey $ toDiagram a
    for_ ns $ \n' -> emitEdge $ Edge n (snd n') ""
    pure $ pure z

instance ToDiagram Int where
  toDiagram = mkText . show

instance ToDiagram a => ToDiagram (Fn a) where
  toDiagram (Fn s vars) =
    mkText s ||| mkText " ↦ " ||| foldr (|||) mempty (fmap go vars)
    where
      go Var = mkText "s"
      go (Lit a) = toDiagram a
      go (Txt a) = mkText a

data Color = Color
  { c_r :: Double
  , c_g :: Double
  , c_b :: Double
  }
  deriving Diagramize via ViaToDiagram Color

instance ToDiagram Color where
  toDiagram (Color r g b) = rect 10 10 & fc (sRGB r g b)

