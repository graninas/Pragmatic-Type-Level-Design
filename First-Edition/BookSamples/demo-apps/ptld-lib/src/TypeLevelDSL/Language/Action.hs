{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Language.Action where

import GHC.TypeLits (Symbol, Nat)


-- Interfaces

data IAction a
data ILambda a

type family MkAction (a :: *) :: IAction a
type family MkLambda (a :: *) :: ILambda a

-- Action implementations

data EndImpl
type End = MkAction EndImpl

data ActionImpl (act :: IAction a) acts
type Action act acts
  = ActionImpl act acts   -- Just a synonym

-- Lambda implementations

data BothImpl lam1 lam2
data PrintImpl
data DropImpl
data ConcatLImpl (str :: Symbol) (lam :: ILambda a)
data ConcatRImpl (lam :: ILambda a) (str :: Symbol)

type Both lam1 lam2  = MkLambda (BothImpl lam1 lam2)
type Print           = MkLambda PrintImpl
type Drop            = MkLambda DropImpl
type ConcatL str lam = MkLambda (ConcatLImpl str lam)
type ConcatR lam str = MkLambda (ConcatRImpl lam str)
