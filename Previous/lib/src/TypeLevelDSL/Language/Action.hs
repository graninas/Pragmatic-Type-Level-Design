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

data ActionTag a
data LambdaTag a

type family MkAction      (a :: *) :: ActionTag a
type family MkLambda      (a :: *) :: LambdaTag a

data Action' (act :: ActionTag a) acts
data Both' lam1 lam2
data End'
data Print'
data Drop'
data ConcatL' (str :: Symbol) (lam :: LambdaTag lamBody)
data ConcatR' (lam :: LambdaTag lamBody) (str :: Symbol)

type Action act acts      = Action' act acts    -- Just a synonym
type End                  = MkAction End'
type Both lam1 lam2       = MkLambda (Both' lam1 lam2)
type Print                = MkLambda Print'
type Drop                 = MkLambda Drop'
type ConcatL str lam      = MkLambda (ConcatL' str lam)
type ConcatR lam str      = MkLambda (ConcatR' lam str)
