{-# LANGUAGE NoMonomorphismRestriction            #-}
{-# LANGUAGE UndecidableInstances                 #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Take2.Computer.CPU.Utils where

import Prelude hiding ((.), id, sum)
import Take2.Computer.Register
import Take2.Computer.Poly
import Circuitry.Machinery


getReg1
    :: Embed a
    => Circuit ((Register, a), Registers PC SP W)
               (W, (a, Registers PC SP W))
getReg1 = swap *** copy
      >>> reassoc'
      >>> second' (reassoc >>> first' (swap >>> getReg))
      >>> reassoc
      >>> first' swap
      >>> reassoc'

