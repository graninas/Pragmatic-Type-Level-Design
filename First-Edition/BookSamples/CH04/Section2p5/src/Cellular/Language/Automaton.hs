{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Cellular.Language.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Common.NonEmptyList
import Cellular.Language.Board
import Cellular.Language.Algorithm


data Topology = Open | Torus
type RuleNameSymb = Symbol
type RuleCodeSymb = Symbol
type RuleName = String
type RuleCode = String


data CellWorld rule where
  CW :: Board -> CellWorld rule

data CustomRule
  (states :: CustomList2 CustomState) where
  Rule
    :: RuleNameSymb
    -> RuleCodeSymb
    -> Neighborhood
    -> CustomStep states
    -> CustomRule states

