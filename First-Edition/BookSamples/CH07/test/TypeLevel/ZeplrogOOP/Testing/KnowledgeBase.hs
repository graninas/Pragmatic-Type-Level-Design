{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module TypeLevel.ZeplrogOOP.Testing.KnowledgeBase where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model

import TypeSelector.Granular

import GHC.TypeLits
import qualified Text.Show as T


type EIcon = Ess @TypeLevel "system:icon"
type EIntrinsics = Ess @TypeLevel "category:intrinsics"

type EWall = Ess @TypeLevel "object:wall"
