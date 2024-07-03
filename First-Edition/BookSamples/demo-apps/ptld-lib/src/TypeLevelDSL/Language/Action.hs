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

data IAction
data ILambda

type family MkAction (a :: *) :: IAction
type family MkLambda (a :: *) :: ILambda

data Both' lam1 lam2
data Print'
data Drop'
data ConcatL' (str :: Symbol) (lam :: ILambda)
data ConcatR' (lam :: ILambda) (str :: Symbol)

type Both lam1 lam2  = MkLambda (Both' lam1 lam2)
type Print           = MkLambda Print'
type Drop            = MkLambda Drop'
type ConcatL str lam = MkLambda (ConcatL' str lam)
type ConcatR lam str = MkLambda (ConcatR' lam str)
