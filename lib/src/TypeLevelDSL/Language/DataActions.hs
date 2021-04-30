{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Language.DataActions where

import TypeLevelDSL.Language.Action

import GHC.TypeLits (Symbol, Nat)



data ReadRef' (refName :: Symbol) (t :: *) (lam :: LambdaTag lamBody)
data WriteRef' (refName :: Symbol) (t :: *)


-- Helpers

type ReadRef n t lam             = MkAction (ReadRef' n t lam)
type WriteRef n t                = MkLambda (WriteRef' n t)
