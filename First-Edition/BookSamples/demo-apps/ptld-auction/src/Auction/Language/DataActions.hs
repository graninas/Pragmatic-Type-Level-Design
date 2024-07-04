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

import Data.HList.HList
import GHC.TypeLits (Symbol, Nat)


-- Interfaces

data ILambda a
type family MkLambda (a :: *) :: ILambda a

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


data LotName
data LotDescr

-- Should valName be a Symbol?
data GetPayloadValueImpl (val :: *) (valType :: *) (lam :: ILambda a)
data GetLotNameImpl (lam :: ILambda a)      -- custom methods
data GetLotDescrImpl (lam :: ILambda a)     -- custom methods


type MkAction act = MkHList act

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

type GetPayloadValue tag typ lam
  = MkAction (GetPayloadValueImpl tag typ lam)
type GetLotName lam
  = MkAction (GetLotNameImpl lam)     -- Custom methods
type GetLotDescr lam
  = MkAction (GetLotDescrImpl lam)    -- Custom methods
type GetLotName2 lam
  = MkAction (GetPayloadValueImpl LotName  String lam)
type GetLotDescr2 lam
  = MkAction (GetPayloadValueImpl LotDescr String lam)
