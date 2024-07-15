module Minefield.Extensions.Implementation where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Interface

import Minefield.Game.Types
import Minefield.Game.System

import Minefield.Extensions.Materialization
import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Verbs.PutFlag

import GHC.TypeLits



instance
  Eval MakeActorAction
       (ObjAct (LandmineImpl ch p) PutFlagImpl)
       ActorAction where
  eval _ _ = pure $ \sysBus pos -> do
    publishEvent sysBus $ ActorRequestEvent pos $ AddOverhaulIcon '🚩'
    publishEvent sysBus $ ActorRequestEvent pos $ SetEnabled False
