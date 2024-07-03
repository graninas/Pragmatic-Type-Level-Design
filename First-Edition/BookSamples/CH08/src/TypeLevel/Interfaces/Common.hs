{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TypeLevel.Interfaces.Common where

import CPrelude

import GHC.TypeLits

-- Interfaces

data IEssence
data IEssencePath

type family MkEssence (a :: *) :: IEssence
type family MkEssencePath (a :: [IEssence]) :: IEssencePath



-- Implementations

data EssenceImpl (ess :: Symbol)
type Essence ess = MkEssence (EssenceImpl ess)

type EssencePath path = MkEssencePath path
