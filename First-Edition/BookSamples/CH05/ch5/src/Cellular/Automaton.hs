{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cellular.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import qualified Data.Map as Map

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton

import Cellular.Implementation.Algorithm


class IAutomaton
  payload
  (rule :: CustomRule) where
  step :: payload -> CellWorld rule -> CellWorld rule
  name :: payload -> Proxy rule -> RuleName
  code :: payload -> Proxy rule -> RuleCode


class IWorld
  payload
  (rule :: CustomRule) where
  initWorld :: payload -> CellWorld rule
  initWorld _ = CW Map.empty


instance
  IAutomaton DynamicRule 'DynRule where
  step dynRule = iterateWorldDyn dynRule
  name (DynamicRule n _ _ _) _ = n
  code (DynamicRule _ c _ _) _ = c


instance
  ( MakeStep step
  , MakeNeighborhoodLookup neighborhood
  , KnownSymbol name
  , KnownSymbol code
  ) =>
  IAutomaton () ('Rule name code neighborhood step) where
  step _ = iterateWorld
  name _ _ = symbolVal (Proxy @name)
  code _ _ = symbolVal (Proxy @code)

instance IWorld () ('Rule name code neighborhood step) where
  {- empty -}

instance IWorld DynamicRule 'DynRule where
  {- empty -}

iterateWorld
  :: forall name code neighborhood step
   . (MakeStep step, MakeNeighborhoodLookup neighborhood)
  => CellWorld ('Rule name code neighborhood step)
  -> CellWorld ('Rule name code neighborhood step)
iterateWorld (CW board) = let
  stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  in CW (stepF board)

iterateWorldDyn
  :: DynamicRule
  -> CellWorld 'DynRule
  -> CellWorld 'DynRule
iterateWorldDyn (DynamicRule _ _ _ _) (CW board) = let
  -- TODO
  -- stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  -- in CW (stepF board)
  in CW board


