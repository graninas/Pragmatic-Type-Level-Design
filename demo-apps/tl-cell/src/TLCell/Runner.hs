module TLCell.Runner where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import TLCell.Types

import TypeLevelDSL.Eval


data Step = Step


instance Eval Step automaton () where
  eval _ _ = ()
