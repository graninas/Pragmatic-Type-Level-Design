module Minefield.Core.Interface where

import CPrelude

import GHC.TypeLits


type OType = String

data IObject where
  ObjectWrapper :: a -> IObject

type family MkObject a :: IObject where
  MkObject a = ObjectWrapper a

type IsCommandDirected = Bool
type BaseCommand = Symbol

data IAction where
  ActionWrapper
    :: a
    -> IsCommandDirected
    -> BaseCommand
    -> IAction

type family MkAction a dir cmd :: IAction where
  MkAction a dir cmd = ActionWrapper a dir cmd

data Game
  (minefield :: [Symbol])
  (player :: IObject)
  (emptyCell :: IObject)
  (supportedObjects :: [IObject])
  (supportedActions :: [IAction])     -- TODO: validate command uniqueness
