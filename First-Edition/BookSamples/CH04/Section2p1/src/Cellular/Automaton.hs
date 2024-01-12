{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import qualified Data.Map as Map

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton

import Cellular.Implementation.Algorithm


class IAutomaton
  (rule :: CustomRule
    (states :: [CustomState])) where
  step :: CellWorld rule -> CellWorld rule
  name :: Proxy rule -> RuleName
  code :: Proxy rule -> RuleCode


class IWorld
    (rule :: CustomRule
      (states :: [CustomState])) where
  initWorld :: CellWorld rule
  initWorld = CW Map.empty


instance
  ( MakeStep step
  , MakeNeighborhoodLookup neighborhood
  , KnownSymbol name
  , KnownSymbol code
  ) =>
  IAutomaton ('Rule name code neighborhood step) where
  step = iterateWorld
  name _ = symbolVal (Proxy @name)
  code _ = symbolVal (Proxy @code)

instance IWorld ('Rule name code neighborhood step) where
  {- empty -}

iterateWorld
  :: forall name code neighborhood step
   . (MakeStep step, MakeNeighborhoodLookup neighborhood)
  => CellWorld ('Rule name code neighborhood step)
  -> CellWorld ('Rule name code neighborhood step)
iterateWorld (CW board) = let
  stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  in CW (stepF board)


