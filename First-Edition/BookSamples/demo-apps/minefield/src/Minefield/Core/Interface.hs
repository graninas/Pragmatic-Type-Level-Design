module Minefield.Core.Interface where

import CPrelude

import Minefield.Core.Types

import GHC.TypeLits


type CommandDef = Symbol

-- | Field object templates that become actors
data IObjectTemplate where
  ObjectTemplateWrapper :: a -> IObjectTemplate

type family MkObjectTemplate a :: IObjectTemplate where
  MkObjectTemplate a = ObjectTemplateWrapper a

-- | Anything the player can do
data IAction where
  ActionWrapper
    :: a
    -> CommandDef
    -> IsDirected
    -> IAction

type family MkAction a cmd dir :: IAction where
  MkAction a cmd dir = ActionWrapper a cmd dir

