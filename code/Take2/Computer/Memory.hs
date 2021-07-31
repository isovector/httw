{-# LANGUAGE UndecidableInstances #-}

module Take2.Computer.Memory where

import Data.Proxy
import Prelude hiding ((.), id, sum)
import Take2.Computer.Simple
import Take2.Computer.Addressed
import Circuitry.Machinery
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import Data.Typeable (Typeable, typeRep)


hold :: Circuit Bool Bool
hold = fixC False $ orGate >>> copy


-- input: R S
rsLatch :: Circuit (Bool, Bool) Bool
rsLatch = component "rs"
         $ fixC False
         $ reassoc'
      >>> second' norGate
      >>> norGate
      >>> copy


-- input: S V
snap :: Circuit (RW, Bool) Bool
snap = component "snap"
     $ unsafeReinterpret @_ @Bool *** (split >>> swap)
   >>> distribP
   >>> both andGate
   >>> rsLatch


snapN
    :: forall a
     . (OkCircuit a, SeparatePorts a, Typeable a)
    => Circuit (RW, a) (Vec (SizeOf a) Bool)
snapN = component ("snap " <> show (typeRep $ Proxy @a))
      $ second' serial
    >>> distribV
    >>> mapV snap


mkRom :: (KnownNat n, Show a, Reify a) => Vec (2 ^ n) a -> Circuit (Addr n) a
mkRom mem
    = decode
  >>> parallelMetaV (fmap (\a -> intro a
                             >>> swap
                             >>> first' serial
                             >>> tribufAll
                          ) mem)
  >>> pointwiseShort
  >>> unsafeParse


data RW = R | W
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving (Embed, Reify, Arbitrary) via (EmbededEnum RW)


data MemoryCommand n a = MemoryCommand
  { mc_rw   :: RW
  , mc_addr :: Addr n
  , mc_data :: a
  }
  deriving stock (Generic, Show)
  deriving anyclass (Reify)

deriving anyclass instance (KnownNat n, Embed a) => Embed (MemoryCommand n a)

instance (KnownNat n, Arbitrary a) => Arbitrary (MemoryCommand n a) where
  arbitrary = genericArbitrary
  shrink = genericShrink


unpackMemoryCommand
    :: (Embed a, KnownNat n, SeparatePorts a)
    => Circuit (MemoryCommand n a) ((Addr n, RW), a)
unpackMemoryCommand
    = copy
  >>> ((copy >>> proj #mc_addr *** (proj #mc_rw >>> serial >>> mapV pullDown >>> unsafeParse)))
  *** proj #mc_data


memoryCell
    :: forall n a
     . (SeparatePorts a, KnownNat n, Embed a, Typeable a)
    => Circuit (MemoryCommand n a) (Vec (SizeOf a) Bool)
memoryCell
    = unpackMemoryCommand
  >>> reassoc'
  >>> addressed
      ( fst'
    >>> first' copy
    >>> reassoc'
    >>> second' snapN
    >>> swap
    >>> second' (unsafeReinterpret @_ @Bool >>> notGate)
    >>> tribufAll
      )

