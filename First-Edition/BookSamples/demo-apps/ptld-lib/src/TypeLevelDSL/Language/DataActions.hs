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


-- Action implementations

data ReadRefImpl
  (refName :: Symbol)
  (t :: *)
  (lam :: ILambda a)
type ReadRef n t lam = MkAction (ReadRefImpl n t lam)

-- Lambda implementaions

data WriteRefImpl
  (refName :: Symbol)
  (t :: *)
type WriteRef n t = MkLambda (WriteRefImpl n t)
