module Minefield.Core.Game where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Interface

import GHC.TypeLits


data GameDef
  (minefield :: [Symbol])
  (player :: IObject)
  (emptyCell :: IObject)
  (supportedObjects :: [IObject])
  (supportedActions :: [IAction])     -- TODO: validate command uniqueness
