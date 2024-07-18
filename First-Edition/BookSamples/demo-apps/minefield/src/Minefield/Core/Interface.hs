module Minefield.Core.Interface where

import CPrelude

import Minefield.Core.Types

import GHC.TypeLits


-- | Field objects that become actors
data IObject where
  ObjectWrapper :: a -> IObject

type family MkObject a :: IObject where
  MkObject a = ObjectWrapper a

type Command = Symbol
type IsDirected = Bool

-- | Anything the player can do
data IAction where
  ActionWrapper
    :: a
    -> Command
    -> IsDirected
    -> IAction

type family MkAction a cmd dir :: IAction where
  MkAction a cmd dir = ActionWrapper a cmd dir

data Game
  (minefield :: [Symbol])
  (player :: IObject)
  (emptyCell :: IObject)
  (supportedObjects :: [IObject])
  (supportedActions :: [IAction])     -- TODO: validate command uniqueness
