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
type RuleName = Symbol
type RuleCode = Symbol


data CellWorld rule where
  CW :: Board -> CellWorld rule

data CustomBoard where
  SquareGrid      -- names of val constr should differ
                  -- to avoid name clash
                  -- with kinds (the compiler gets confused)
    :: Topology
    -> CustomBoard

data CustomRule
  (board :: CustomBoard) where
  Rule
    :: RuleName
    -> RuleCode
    -> CustomBoard
    -> Neighborhood
    -> CustomStep
    -> CustomRule board

