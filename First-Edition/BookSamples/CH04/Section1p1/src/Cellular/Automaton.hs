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
    (board :: CustomBoard)) where
  step :: CellWorld rule -> CellWorld rule
  name :: Proxy rule -> RuleName
  code :: Proxy rule -> RuleCode


class IWorld
    (rule :: CustomRule
      (board :: CustomBoard)) where
  initWorld :: CellWorld rule
  initWorld = CW Map.empty


instance
  (MakeStep step, MakeNeighborhoodLookup neighborhood,
   KnownSymbol name, KnownSymbol code) =>
  IAutomaton ('Rule name code board neighborhood step) where
  step = iterateWorld
  name _ = symbolVal (Proxy @name)
  code _ = symbolVal (Proxy @code)

instance IWorld ('Rule name code board neighborhood step) where
  {- empty -}

iterateWorld
  :: forall name code board neighborhood step
   . (MakeStep step, MakeNeighborhoodLookup neighborhood)
  => CellWorld ('Rule name code board neighborhood step)
  -> CellWorld ('Rule name code board neighborhood step)
iterateWorld (CW board) = let
  stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  in CW (stepF board)


