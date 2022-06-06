{-# OPTIONS_GHC -Wno-orphans #-}

module Languages.Tree where

import Data.String
import Types
import Dot


instance IsString a => IsString (LRose a) where
  fromString = LPure . fromString

instance (b ~ [LRose a], IsString a) => IsString (b -> LRose a) where
  fromString a x = LRose (fromString a) x

asRose :: LRose String -> LRose String
asRose = id

