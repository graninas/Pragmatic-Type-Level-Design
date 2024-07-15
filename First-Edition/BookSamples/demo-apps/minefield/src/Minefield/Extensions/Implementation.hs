module Minefield.Extensions.Implementation where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface

import Minefield.Game.Types
import Minefield.Game.System

import Minefield.Extensions.Materialization
import Minefield.Extensions.Nouns.Landmine
import Minefield.Extensions.Verbs.PutFlag

import GHC.TypeLits


instance
  ( obj ~ LandmineImpl ch ot p
  , Eval () GetObjectType obj String
  ) =>
  Eval () MakeActorAction
       (ObjAct obj PutFlagImpl)
       (ObjectType, ActorAction) where
  eval () _ _ = do
    oType <- eval () (Proxy @GetObjectType) $ Proxy @obj

    let act = \sysBus pos -> do
          publishEvent sysBus $ ActorRequestEvent oType pos $ AddOverhaulIcon '🚩'
          publishEvent sysBus $ ActorRequestEvent oType pos $ SetEnabled False

    pure (oType, act)
