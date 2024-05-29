{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeLevel.ZeplrogOOP.Testing.Utils where

import CPrelude

import TypeLevel.System.Debug
import TypeLevel.ZeplrogOOP.Static.Model

import TypeSelector.Granular

import Data.Proxy
import qualified Data.Map.Strict as Map
import GHC.TypeLits


sMatEss
  :: forall ess symb
  . KnownSymbol symb
  => (ess ~ 'Ess @TypeLevel symb)
  => EssenceVL
sMatEss = Ess $ symbolVal (Proxy @symb)
