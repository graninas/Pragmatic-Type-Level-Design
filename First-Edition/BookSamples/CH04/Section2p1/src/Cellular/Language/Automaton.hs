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

data CustomRule (states :: [CustomState]) where
  Rule
    :: RuleNameSymb
    -> RuleCodeSymb
    -> Neighborhood
    -> CustomStep states
    -> CustomRule states
