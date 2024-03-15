module SymbolIsNotString where

import GHC.TypeLits ( Symbol, Nat )


-- This snippet demonstrates that value-level strings
-- of type String can't be used as Symbol values
-- (at least without OverloadedStrings in which I'm not sure either).

-- In the same time, value-level numbers can be used as Nat.


newtype MyADT1 = MkMyADT1 Symbol

-- myADT1 :: MyADT1
-- myADT1 = MkMyADT1 ""     -- won't compile: String is not Symbol


newtype MyADT2 = MkMyADT2 Nat

myADT2 :: MyADT2
myADT2 = MkMyADT2 3         -- compiles fine: a number is Nat
