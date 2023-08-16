{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module ExtensionsTest where

import GHC.TypeLits (Symbol)

data StandardHaskell someType = Test0
data NeedKindSignatures (a :: *) = Test1
data NeedKindSignaturesAndDataKinds (a :: Symbol) = Test2
