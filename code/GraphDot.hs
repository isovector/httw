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

class GDiagramize f x where
  gdiagramize :: f x -> RWS (Diagram B -> Diagram B) [Either (Node, Diagram B) Edge] Node [(String, Node)]

instance GDiagramize f x => GDiagramize (D1 _1 f) x where
  gdiagramize (M1 f) = gdiagramize f

instance (KnownSymbol name, GDiagramize f x) => GDiagramize (S1 ('MetaSel ('Just name) _1 _2 _3) f) x where
  gdiagramize (M1 f) = fmap (fmap $ first $ const $ symbolVal $ Proxy @name) $ gdiagramize f

instance GDiagramize f x => GDiagramize (S1 ('MetaSel 'Nothing _1 _2 _3) f) x where
  gdiagramize (M1 f) = gdiagramize f

instance {-# OVERLAPPABLE #-} (KnownSymbol name, GDiagramize f x) => GDiagramize (C1 ('MetaCons name _2 _3) f) x where
  gdiagramize (M1 f) = do
    ns <- gdiagramize f
    r@(_, n) <- emitNode $ mkText $ nameof @name
      -- circle 3 & center & fc black
    for_ ns $ \(e, n') -> emitEdge $ Edge n n' e
    pure $ pure r

instance {-# OVERLAPPING #-} (KnownSymbol name, Diagramize a) => GDiagramize (C1 ('MetaCons name _2 _3) (M1 _4 _5 Par1)) (ExpandGraph a) where
  gdiagramize (M1 (M1 (Par1 f))) = do
    ns <- diagramize f
    r@(_, n) <- emitNode $ mkText $ nameof @name
      -- circle 3 & center & fc black
    for_ ns $ \(e, n') -> emitEdge $ Edge n n' e
    pure $ pure r

instance (KnownSymbol name, ToDiagram a) => GDiagramize (C1 ('MetaCons name _2 _3) (M1 _4 _5 (K1 _6 a))) x where
  gdiagramize (M1 (M1 (K1 a))) = do
    fmap pure $ emitNode $ mkText (nameof @name <> " ") ||| toDiagram a

instance (KnownSymbol name, ToDiagram x) => GDiagramize (C1 ('MetaCons name _2 _3) (M1 _4 _5 Par1)) x where
  gdiagramize (M1 (M1 (Par1 a))) = do
    fmap pure $ emitNode $ mkText (nameof @name <> " ") ||| toDiagram a

nameof :: forall name. KnownSymbol name => String
nameof =
  case symbolVal $ Proxy @name of
    ":" -> "Cons"
    "[]" -> "Nil"
    x -> x


instance GDiagramize U1 x where
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

instance (GDiagramize f x, GDiagramize g x) => GDiagramize (f :*: g) x where
  gdiagramize (f :*: g) = do
    fn <- gdiagramize f
    gn <- gdiagramize g
    -- n <- emitNode $ circle 3 # center # fc black
    -- emitEdge $ Edge n fn ""
    -- emitEdge $ Edge n gn ""
    pure $ fn <> gn

instance (GDiagramize f x, GDiagramize g x) => GDiagramize (f :+: g) x where
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

instance ToDiagram a => GDiagramize Par1 a where
  gdiagramize = fmap pure . emitNode . toDiagram . unPar1

instance ToDiagram a => GDiagramize (K1 _1 a) x where
  gdiagramize = fmap pure . emitNode . toDiagram . unK1

instance Diagramize (f x) => GDiagramize (Rec1 f) x where
  gdiagramize = diagramize . unRec1


newtype ViaDiagramize a = ViaDiagramize { unViaDiagramize :: a }

mkNode :: (Node, a) -> (Node, CmpFst Node a)
mkNode (n, a) = (n, CmpFst n a)

instance (Generic a, GDiagramize (Rep a) ()) => Diagramize (ViaDiagramize a) where
  diagramize = gdiagramize @_ @() . from . unViaDiagramize

instance {-# OVERLAPPING #-} (Generic1 f, GDiagramize (Rep1 f) a) => Diagramize (ViaDiagramize (f a)) where
  diagramize = gdiagramize . from1 . unViaDiagramize

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
    pure $ center $ bgFrameA 10 grey 0.5 $
      drawGraph
        (\d -> place (cf_snd d))
        (\_ p1 _ p2 e p -> arrowBetween' (opts p e) p1 p2)
        g'
  {-# NOINLINE toDiagram #-}

bgFrameA :: (TypeableFloat n, Renderable (Path V2 n) b, Monoid' q)
    => n -> Colour Double -> Double -> QDiagram b V2 n q -> QDiagram b V2 n q
bgFrameA f c a d = d <> boundingRect (frame f d) # lwO 0 # fc c # opacity a # value mempty

deriving via ViaDiagramize (Bin a) instance ToDiagram a => Diagramize (Bin a)
deriving  via ViaDiagramize (Bin (ExpandGraph a)) instance {-# OVERLAPPING #-} Diagramize a => Diagramize (Bin (ExpandGraph a))
deriving via ViaDiagramize (Maybe a) instance ToDiagram a => Diagramize (Maybe a)
deriving via ViaDiagramize (a, b) instance (ToDiagram a, ToDiagram b) => Diagramize (a, b)
-- deriving via ViaDiagramize () instance Diagramize ()

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

instance ToDiagram Double where
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
  deriving stock (Generic)
  -- deriving Diagramize via (ViaDiagramize Color)

instance Diagramize Color where
  diagramize = gdiagramize @_ @() . from

instance ToDiagram Color where
  toDiagram (Color r g b) = rect 10 10 & fc (sRGB r g b)

newtype ExpandGraph a = Expand { getExpandGraph :: a }
  deriving stock (Eq, Ord, Show, Functor, Generic, Generic1)

instance Diagramize a => Diagramize (ExpandGraph a) where
  diagramize = diagramize . getExpandGraph

asRed :: Double -> Color
asRed r = Color r 0 0


