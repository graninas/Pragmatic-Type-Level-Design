{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeLevel.ZeplrogOOP.Testing.Utils where

import CPrelude

import TypeLevel.System.Debug
import TypeLevel.ZeplrogOOP.Static.Model
import qualified TypeLevel.ZeplrogOOP.Dynamic.Model as DMod

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

instEss
  :: EssenceVL
  -> DMod.Essence
instEss (Ess ess) = ess

toDynEss
  :: forall ess symb
  . KnownSymbol symb
  => (ess ~ 'Ess @TypeLevel symb)
  => DMod.Essence
toDynEss = instEss $ sMatEss @ess




class AuxMat it to | it -> to where
  auxMat :: Proxy it -> to

data AuxEsss esss

instance
  AuxMat (AuxEsss '[]) [EssenceVL] where
  auxMat _ = []

instance
  ( KnownSymbol symb
  , ess ~ 'Ess @TypeLevel symb
  , AuxMat (AuxEsss esss) [EssenceVL]
  ) =>
  AuxMat (AuxEsss (ess ': esss))
         [EssenceVL] where
  auxMat _ = let
    ess = symbolVal $ Proxy @symb
    esss = auxMat $ Proxy @(AuxEsss esss)
    in Ess ess : esss

toDynEsss
  :: forall esss
  . AuxMat (AuxEsss esss) [EssenceVL]
  => [DMod.Essence]
toDynEsss = map instEss $ auxMat $ Proxy @(AuxEsss esss)

