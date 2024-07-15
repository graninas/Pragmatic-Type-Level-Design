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
  Eval MakeGameAction
       (ObjAct (LandmineImpl ch p) PutFlagImpl)
       GameAction where
  eval _ _ = do
    pure $ \sysBus pos -> do
      publishEvent sysBus $ ActorEvent pos $ AddOverhaulIcon '🚩'
      publishEvent sysBus $ ActorEvent pos $ SetEnabled False
