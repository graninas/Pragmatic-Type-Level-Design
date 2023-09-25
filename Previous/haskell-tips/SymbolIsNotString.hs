module SymbolIsNotString where

import GHC.TypeLits ( Symbol, Nat )

newtype MyADT1 = MkMyADT1 Symbol

-- myADT1 :: MyADT1
-- myADT1 = MkMyADT1 ""     -- won't compile: String is not Symbol

newtype MyADT2 = MkMyADT2 Nat

myADT2 :: MyADT2
myADT2 = MkMyADT2 3  -- compiles fine
