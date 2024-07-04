{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Data.HList.HList where

import GHC.TypeLits (Symbol, Nat)


-- Interface

data IHList a

type family MkHList (a :: *) :: IHList a

-- Implementation

data HEmptyImpl
type HEmpty = MkHList HEmptyImpl
type End = HEmpty

data HListImpl it its
type HList it its = HListImpl it its   -- Just a synonym

type (:>) it its = HList it its

infixr 6 :>
