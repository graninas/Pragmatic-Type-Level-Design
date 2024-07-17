{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module ZeplrogOOP.Static.Model.Common where

import CPrelude

import GHC.TypeLits


-- Interfaces

data IEssence where
  EssenceWrapper :: a -> IEssence

data IEssencePath where
  EssencePathWrapper :: a -> IEssencePath

type family MkEssence (a :: *) :: IEssence where
  MkEssence a = 'EssenceWrapper a

type family MkEssencePath (a :: [IEssence]) :: IEssencePath where
  MkEssencePath a = 'EssencePathWrapper a


-- Implementations

data EssenceImpl (ess :: Symbol)
type Essence ess = MkEssence (EssenceImpl ess)
type EssencePath path = MkEssencePath path
