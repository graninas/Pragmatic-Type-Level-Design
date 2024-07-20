{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Language.DataActions where

import GHC.TypeLits (Symbol, Nat)


-- Interfaces

data ILambda inT outT where
  LambdaWrapper :: a -> ILambda inT outT

type family MkLambda inT outT a :: ILambda inT outT where
  MkLambda inT outT a = LambdaWrapper a

data IAction where
  ActionWrapper :: lam -> IAction

type family MkAction lam :: IAction where
  MkAction lam = ActionWrapper lam

-- | Denotes a value with name, type-level type and runtime type.
data ITag where
  TagWrapper :: Symbol -> t -> dynT -> ITag

type family MkTag (name :: Symbol) t dynT :: ITag where
  MkTag name t dynT = TagWrapper name t dynT

-- Lambda & action implementations

data BothImpl
  (lam1 :: ILambda inT outT)
  (lam2 :: ILambda inT outT)

-- Not implemented yet
data IfImpl
  (cond :: Bool)
  (then_ :: ILambda inT outT)
  (else_ :: ILambda inT outT)

data ShowFImpl
  (lam :: ILambda Symbol outT)

data PrintFImpl

data PrintLineImpl
  (str :: Symbol)

data GetLineImpl
  (lam :: ILambda Symbol outT)

data ConcatLImpl
  (str :: Symbol)
  (lam :: ILambda Symbol outT)

data ConcatRImpl
  (str :: Symbol)
  (lam :: ILambda Symbol outT)

-- Not implemented yet
type If
  (cond :: Bool)
  (then_ :: ILambda inT outT)
  (else_ :: ILambda inT outT)
  = MkLambda inT outT (IfImpl cond then_ else_)

type Both
  (lam1 :: ILambda inT outT)
  (lam2 :: ILambda inT outT)
  = MkLambda inT outT (BothImpl lam1 lam2)

type GetLine
  (lam :: ILambda Symbol outT)
  = MkAction (GetLineImpl lam)

type PrintLine
  (str :: Symbol)
  = MkAction (PrintLineImpl str)

type ShowF
  inT
  (lam :: ILambda Symbol outT)
  = MkLambda inT outT (ShowFImpl lam)

type PrintF = MkLambda Symbol () PrintFImpl

type ConcatL
  (str :: Symbol)
  (lam :: ILambda Symbol outT)
  = MkLambda Symbol Symbol (ConcatLImpl str lam)

type ConcatR
  (str :: Symbol)
  (lam :: ILambda Symbol outT)
  = MkLambda Symbol Symbol (ConcatRImpl str lam)

data ReadRefImpl
  (rtT :: *)
  (refName :: Symbol)
  (lam :: ILambda rtT outT)

data WriteRefImpl
  (rtT :: *)
  (refName :: Symbol)

type ReadRef
  (rtT :: *)
  (refName :: Symbol)
  (lam :: ILambda rtT outT)
  = MkAction (ReadRefImpl rtT refName lam)

type WriteRef
  (rtT :: *)
  (refName :: Symbol)
  = MkLambda rtT () (WriteRefImpl rtT refName)

data GetPayloadValueImpl
  (tag :: ITag)
  (lam :: ILambda inT outT)

type GetPayloadValue
  (tag :: ITag)
  (lam :: ILambda inT outT)
  = MkAction (GetPayloadValueImpl tag lam)

