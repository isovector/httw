{-# LANGUAGE OverloadedStrings             #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Take2.Computer.Math where

import GHC.TypeLits
import           Prelude hiding ((.), id, sum)
import           Circuitry.Machinery
import qualified Circus.Types as Y
import qualified Clash.Sized.Vector as V
import qualified Data.Map as M
import Circus.DSL
import Circuitry.Graph (Graph(Graph))
import Take2.Computer.Simple (bigNotGate)


everyPair
    :: (OkCircuit a, OkCircuit b, OkCircuit c)
    => Circuit (a, (b, c))
               ((a, b), ((a, c), (b, c)))
everyPair = (reassoc >>> fst')
       &&& ((second' swap >>> reassoc >>> fst') &&& snd')


cout :: Circuit (Bool, (Bool, Bool)) Bool
cout = everyPair
   >>> andGate *** (andGate *** andGate)
   >>> serial
   >>> bigOrGate


sum :: Circuit (Bool, (Bool, Bool)) Bool
sum = second' xorGate >>> xorGate


-- input: A B Cin
-- output: S Cout
add2 :: Circuit (Bool, (Bool, Bool)) (Bool, Bool)
add2
    = coerceC
    $ component "full adder"
    $ unsafeReinterpret @(Named "A" Bool, (Named "B" Bool, Named "Cin" Bool))
  >>> copy
  >>> sum *** cout
  >>> unsafeReinterpret @_ @(Named "A+B" Bool, Named "Cout" Bool)


addN
    :: forall a
     . (SeparatePorts a, Numeric a, Reify a)
    => Circuit (a, a) (a, Bool)
addN = diagrammed gr
     $ shortcircuit (uncurry addNumeric)
     $ serial *** serial
   >>> zipVC
   >>> create
   >>> second' (constC False)
   >>> mapFoldVC (reassoc' >>> add2)
   >>> first' unsafeParse
  where
    gr = Graph $ \i -> do
      let (a, b) = V.splitAtI @(SizeOf a) i
      res <- synthesizeBits @a
      c <- freshBit
      addCell $ Y.mkCell Y.CellAdd $ M.fromList
        [ ("A", (Y.Input, V.toList a))
        , ("B", (Y.Input, V.toList b))
        , ("Y", (Y.Output, V.toList res))
        , ("Cout", (Y.Output, [c]))
        ]
      pure (res V.++ (c V.:> V.Nil))

mnegate :: KnownNat n => Circuit (Bool, Vec n Bool) (Vec n Bool)
mnegate
    = component "mnegate"
    $ second' (copy >>> first' bigNotGate)
  >>> distribP
  >>> second' (first' notGate)
  >>> both (swap >>> tribufAll)
  >>> pairwiseShort

addsubN
    :: forall a
     . (SeparatePorts a, Numeric a, OkCircuit a)
    => Circuit (Bool, (a, a)) (a, Bool)
addsubN = diagrammed gr
     $ second' (both serial >>> swap)
   >>> reassoc
   >>> first' (first' copy >>> reassoc' >>> second' mnegate)
   >>> reassoc'
   >>> second' (swap >>> zipVC)
   >>> swap
   >>> mapFoldVC (reassoc' >>> add2)
   >>> first' unsafeParse
  where
    gr :: Graph (Bool, (a, a)) (a, Bool)
    gr = Graph $ \(n :> (i :: Vec (SizeOf a * 2) Y.Bit)) -> do
      let (a, b) = V.splitAtI @(SizeOf a) @(SizeOf a) i
      res <- synthesizeBits @a
      c <- freshBit
      addCell $ Y.mkCell (Y.CellGeneric "$addsub") $ M.fromList
        [ ("A", (Y.Input, V.toList a))
        , ("B", (Y.Input, V.toList b))
        , ("S", (Y.Input, [n]))
        , ("Y", (Y.Output, V.toList res))
        , ("Cout", (Y.Output, [c]))
        ]
      pure (res V.++ (c V.:> V.Nil))




shiftL :: forall a. (1 <= SizeOf a, Embed a, Numeric a) => Circuit a a
shiftL = unsafeReinterpret @_ @(Vec (SizeOf a - 1) Bool, Bool)
     >>> fst'
     >>> create
     >>> swap
     >>> first' (constC $ zero @Bool)
     >>> unsafeReinterpret


shiftR :: forall a. (1 <= SizeOf a, Embed a, Numeric a) => Circuit a a
shiftR = serial
     >>> unconsC @(SizeOf a - 1)
     >>> snd'
     >>> create
     >>> second' (constC $ zero @Bool)
     >>> unsafeReinterpret


ashiftR :: forall a. (2 <= SizeOf a, Embed a, Numeric a) => Circuit a a
ashiftR = serial
     >>> unconsC @(SizeOf a - 1)
     >>> snd'
     >>> unsafeReinterpret @_ @(Vec (SizeOf a - 2) Bool, Bool)
     >>> second' copy
     >>> unsafeReinterpret
