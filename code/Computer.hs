{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Computer where

import Prelude hiding (sum)
import Circuitry.Catalyst
import Control.Category.Recursive



zipV :: Vector n a -> Vector n b -> Vector n (a, b)
zipV Empty Empty = Empty
zipV (a' :| vec) (b' :| vec') = (a', b') :| zipV vec vec'


foldrC
    :: Circuit () r
    -> Circuit r ()
    -> Circuit a (b, r)
    -> Circuit (Vector n a) (Vector n b)
foldrC create destroy one = recurseL $ _



pairwise :: Circuit (a, (b, c)) ((a, b), ((a, c), (b, c)))
pairwise = copy
    >>> (reassoc >>> fst')
    *** (copy
          >>> (second' swap >>> reassoc >>> fst') *** snd'
        )


distrib :: Circuit (a, (b, c)) ((a, b), (a, c))
distrib
    = copy
  >>> (reassoc >>> fst')
  *** (second' swap >>> reassoc >>> fst')


paired :: Circuit (a, (b, c)) ((a, b), (b, c))
paired
    = copy
  >>> (reassoc >>> fst') *** snd'


cout :: Circuit ((Bool, Bool), Bool) Bool
cout = reassoc'
   >>> pairwise
   >>> andGate *** (andGate *** andGate)
   >>> paired
   >>> orGate *** orGate
   >>> orGate


sum :: Circuit ((Bool, Bool), Bool) Bool
sum = first' xorGate >>> xorGate


add :: Circuit ((Bool, Bool), Bool) (Bool, Bool)
add = copy >>> sum *** cout

