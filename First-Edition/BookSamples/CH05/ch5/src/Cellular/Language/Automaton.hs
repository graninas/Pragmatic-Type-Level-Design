{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Language.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Cellular.Language.Board
import Cellular.Language.Algorithm


data Topology = Open | Torus
type RuleNameSymb = Symbol
type RuleCodeSymb = Symbol
type RuleName = String
type RuleCode = String


data CellWorld rule where
  CW :: Board -> CellWorld rule

data CustomRule where
  -- | Static type-level rule
  Rule
    :: RuleNameSymb
    -> RuleCodeSymb
    -> Neighborhood
    -> CustomStep states   -- N.B., `states` can be absent in the rule itself
    -> CustomRule
  -- | Dynamic value-level rule
  DynRule
    :: CustomRule

data DynamicRule where
  DynamicRule
    :: RuleName
    -> RuleCode
    -> Neighborhood
    -> DynamicStep
    -> DynamicRule

