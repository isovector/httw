{-# LANGUAGE UndecidableInstances                 #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Take2.Computer.ALU where

import Data.Typeable
import Prelude hiding ((.), id, sum)
import Take2.Computer.Math
import Circuitry.Machinery
import Test.QuickCheck.Arbitrary.Generic
import Take2.Computer.Simple (bigNotGate)


data AluCommand a
  = AluAdd a a
  | AluSub a a
  | AluAnd a a
  | AluOr a a
  | AluXor a a
  | AluNot a
  | AluShiftL a
  | AluShiftR a
  | AluAShiftR a
  deriving stock (Show, Generic)
  deriving anyclass (Reify)

deriving anyclass instance KnownNat (SizeOf a) => Embed (AluCommand a)

instance Arbitrary a => Arbitrary (AluCommand a) where
  arbitrary = genericArbitrary
  shrink    = genericShrink


alu
    :: forall a
     . (2 <= SizeOf a, SeparatePorts a, Reify a, Numeric a, Typeable a)
    => Circuit (AluCommand a) (Vec (SizeOf a) Bool)
alu = create
  >>> (second' $ share $ addsubN @a)
  >>> elim ( foldElim
        $ #_AluAdd :-> reassoc
                   >>> swap
                   >>> second' (first' (intro False >>> swap >>> tribuf))
                   >>> eval
                   >>> fst'
                   >>> serial
      :+| #_AluSub :-> reassoc
                   >>> swap
                   >>> second' (first' (intro True >>> swap >>> tribuf))
                   >>> eval
                   >>> fst'
                   >>> serial
      :+| #_AluAnd :->
            snd' >>> fst' >>> both serial >>> pointwise andGate
      :+| #_AluOr :->
            snd' >>> fst' >>> both serial >>> pointwise orGate
      :+| #_AluXor :->
            snd' >>> fst' >>> both serial >>> pointwise xorGate
      :+| #_AluNot :->
            snd' >>> fst' >>> serial >>> bigNotGate
      :+| #_AluShiftL :->
            snd' >>> fst' >>> shiftL >>> serial
      :+| #_AluShiftR :->
            snd' >>> fst' >>> shiftR >>> serial
      :+| #_AluAShiftR :->
            snd' >>> fst' >>> ashiftR >>> serial
      :+| End
        )

