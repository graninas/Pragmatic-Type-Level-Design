module Minefield.Core.Language where

import CPrelude

import GHC.TypeLits


data IObject where
  ObjectWrapper :: a -> IObject

type family MkObject a :: IObject where
  MkObject a = ObjectWrapper a



data IAction where
  ActionWrapper :: a -> IAction

type family MkAction a :: IAction where
  MkAction a = ActionWrapper a


data Direction = U | D | L | R

data Game
  (minefield :: [Symbol])
  (player :: IObject)
  (emptyCell :: IObject)
  (supportedObjects :: [IObject])
  (supportedActions :: [IAction])
