{-# LANGUAGE OverloadedStrings #-}

module Take2.Computer.Simple where

import qualified Clash.Sized.Vector as V
import           Prelude hiding ((.), id, sum)
import           Circuitry.Machinery
import           Circuitry.Primitives (gateDiagram, constantName)
import qualified Circus.Types as Y



cut :: Embed a => Circuit a ()
cut = create >>> snd'


split :: Circuit Bool (Bool, Bool)
split = copy >>> second' notGate


andV :: KnownNat n => Circuit (Bool, Vec n Bool) (Vec n Bool)
andV = component "andV" $ distribV >>> mapV andGate


bigAndGate :: KnownNat n => Circuit (Vec n Bool) Bool
bigAndGate
  = shortcircuit (V.foldr (&&) True)
  $ gateDiagram (unaryGateDiagram' "C" Y.CellAnd)
  $ create >>> second' (constC True) >>> foldVC andGate


bigNotGate :: KnownNat n => Circuit (Vec n Bool) (Vec n Bool)
bigNotGate
  = gateDiagram (unaryGateDiagram Y.CellNot)
  $ mapV notGate


pointwiseAnd
    :: (KnownNat n, KnownNat m)
    => Circuit (Vec m (Vec n Bool)) (Vec n Bool)
pointwiseAnd = transposeV >>> mapV bigAndGate

pointwiseOr
    :: (1 <= m, KnownNat n, KnownNat m)
    => Circuit (Vec m (Vec n Bool)) (Vec n Bool)
pointwiseOr = transposeV >>> mapV bigOrGate


eq :: (Embed a) => Circuit (a, a) Bool
eq = diagrammed (binaryGateDiagram Y.CellEq)
   $ both serial >>> zipVC >>> mapV nxorGate >>> bigAndGate


ifOrEmpty :: (Embed a, Embed b) => Circuit a b -> Circuit (Bool, a) (Vec (SizeOf b) Bool)
ifOrEmpty c = second' (c >>> serial) >>> andV


when
    :: (Reify k, Embed v, Embed r, Show k, SeparatePorts k, SeparatePorts v)
    => k
    -> Circuit v r
    -> Circuit (k, v) (Vec (SizeOf r) Bool)
when k c = interface' (mkPort "i") (mkPort "o") diagrammed (fmap (mappend "case ") $ constantName k)
           (first' (intro k >>> eq))
       >>> ifOrEmpty c

when'
    :: (Reify k, Embed v, Embed r, Show k, SeparatePorts k, SeparatePorts v)
    => k
    -> Circuit (Bool, v) r
    -> Circuit (k, v) (Vec (SizeOf r) Bool)
when' k c = interface' (mkPort "i") (mkPort "o") diagrammed (fmap (mappend "case ") $ constantName k)
           (first' (intro k >>> eq))
       >>> c
       >>> serial


onEach :: (Embed a, Embed b, KnownNat cases) => (v -> Circuit a b) -> Vec cases v -> Circuit a (Vec cases b)
onEach f v = sequenceMetaV $ fmap f v


sext
    :: forall n m
     . (KnownNat m, KnownNat n, 1 <= m, (m - 1) <= n)
    => Circuit (Vec m Bool) (Vec n Bool)
sext
    = separate @(m - 1)
  >>> second' (unsafeParse @Bool >>> replicateC @(n - (m - 1)))
  >>> unsafeReinterpret


ext
    :: forall m n
     . (KnownNat m, KnownNat n, m <= n)
    => Circuit (Vec m Bool) (Vec n Bool)
ext = pad False >>> unsafeReinterpret

