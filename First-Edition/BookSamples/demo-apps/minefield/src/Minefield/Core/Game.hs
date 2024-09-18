module Minefield.Core.Game where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Interface

import GHC.TypeLits


data GameDef
  (minefield :: [Symbol])
  (player :: IObjectTemplate)
  (emptyCell :: IObjectTemplate)
  (supportedObjects :: [IObjectTemplate])
  (supportedActions :: [IAction])     -- TODO: validate command uniqueness
