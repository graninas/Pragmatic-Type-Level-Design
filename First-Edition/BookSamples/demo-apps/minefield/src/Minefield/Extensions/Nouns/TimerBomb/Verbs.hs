module Minefield.Extensions.Nouns.TimerBomb.Verbs where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.System.Types
import Minefield.Core.System.Actor
import Minefield.Core.System.Event

import Minefield.Extensions.Nouns.TimerBomb.Def
import Minefield.Implementation.Commons
import Minefield.Implementation.SystemVerbs.PutFlag
import Minefield.Implementation.Materialization

import GHC.TypeLits


-- TimerBomb verbs implementation

-- -- PutFlag
instance
  ( KnownSymbol ot
  ) =>
  EvalIO () MakeActorAction
       (ObjAct (TimerBombDef i ot p) PutFlagImpl)
       (ObjectType, ActorAction) where
  evalIO () _ _ = do
    let oType = symbolVal $ Proxy @ot
    pure (oType, disableBombEffect)
