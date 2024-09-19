module Minefield.Extensions.Nouns.Landmine.Verbs where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Extensions.Nouns.Landmine.Def
import Minefield.Implementation.Commons
import Minefield.Implementation.SystemVerbs.PutFlag
import Minefield.Implementation.Materialization

import GHC.TypeLits


-- Landmine verbs implementation

instance
  ( KnownSymbol ot
  -- For some reason, this syntax is not universal.
  -- It prevents other objects from being accepted
  -- when unwrapping from ObjectWrapper.
  -- , obj ~ LandmineDef i ot p
  ) =>
  EvalIO () MakeActorAction
       (ObjAct (LandmineDef i ot p) PutFlagDef)
       (ObjectType, ActorAction) where
  evalIO () _ _ = do
    let oType = symbolVal $ Proxy @ot
    pure (oType, disableBombEffect)
