{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Materialization where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Game.Types
import Minefield.Game.UI

import GHC.TypeLits
import qualified Data.Map as Map


data GetIcon         = GetIcon
data GetObjectInfo   = GetObjectInfo
data GetObjectType   = GetObjectType
data MakeActorAction = MakeActorAction
data MakeGameAction  = MakeGameAction
data MakeGameActions = MakeGameActions
data MakeActors      = MakeActors
data MakeActor       = MakeActor
data GetIsDirected   = GetIsDirected

data Objects (a :: [IObject])

data ObjsActs objs acts
data TraverseObjs objs acts
data TraverseActs obj acts
data ObjAct o a

-- Get icon

instance
  ( EvalIO () GetIcon o Char
  ) =>
  EvalIO () GetIcon ('ObjectWrapper o) Char where
  evalIO () verb _ = evalIO () verb $ Proxy @o

instance
  EvalIO () GetIcon (Objects '[]) [Char] where
  evalIO () _ _ = pure []

instance
  ( EvalIO () GetIcon o Char
  , EvalIO () GetIcon (Objects os) [Char]
  ) =>
  EvalIO () GetIcon (Objects (o ': os)) [Char] where
  evalIO () verb _ = do
    o  <- evalIO () verb $ Proxy @o
    os <- evalIO () verb $ Proxy @(Objects os)
    pure $ o : os

-- Get object info

instance
  ( EvalIO () GetObjectInfo o ObjectInfo
  ) =>
  EvalIO () GetObjectInfo ('ObjectWrapper o) ObjectInfo where
  evalIO () verb _ = evalIO () verb $ Proxy @o

instance
  EvalIO () GetObjectInfo (Objects '[]) [ObjectInfo] where
  evalIO () _ _ = pure []

instance
  ( EvalIO () GetObjectInfo o ObjectInfo
  , EvalIO () GetObjectInfo (Objects os) [ObjectInfo]
  ) =>
  EvalIO () GetObjectInfo (Objects (o ': os)) [ObjectInfo] where
  evalIO () verb _ = do
    o  <- evalIO () verb $ Proxy @o
    os <- evalIO () verb $ Proxy @(Objects os)
    pure $ o : os

-- Get object type

instance
  ( EvalIO () GetObjectType o ObjectType
  ) =>
  EvalIO () GetObjectType ('ObjectWrapper o) ObjectType where
  evalIO () verb _ = evalIO () verb $ Proxy @o

instance
  EvalIO () GetObjectType (Objects '[]) [ObjectType] where
  evalIO () _ _ = pure []

instance
  ( EvalIO () GetObjectType o ObjectType
  , EvalIO () GetObjectType (Objects os) [ObjectType]
  ) =>
  EvalIO () GetObjectType (Objects (o ': os)) [ObjectType] where
  evalIO () verb _ = do
    o  <- evalIO () verb $ Proxy @o
    os <- evalIO () verb $ Proxy @(Objects os)
    pure $ o : os

-- Make game action

instance
  ( EvalIO () MakeGameActions (TraverseObjs os acts) GameActions
  ) =>
  EvalIO () MakeGameActions (ObjsActs os acts) GameActions where
  evalIO () verb _ =
    evalIO () verb $ Proxy @(TraverseObjs os acts)

instance
  EvalIO () MakeGameActions (TraverseObjs '[] acts) GameActions where
  evalIO () _ _ = pure Map.empty

instance
  ( EvalIO () MakeGameActions (TraverseObjs os acts) GameActions
  , EvalIO () MakeGameActions (TraverseActs o acts) GameActions
  ) =>
  EvalIO () MakeGameActions (TraverseObjs (o ': os) acts) GameActions where
  evalIO () verb _ = do
    act1 <- evalIO () verb $ Proxy @(TraverseActs o acts)
    act2 <- evalIO () verb $ Proxy @(TraverseObjs os acts)
    pure $ Map.union act1 act2

instance
  EvalIO () MakeGameActions (TraverseActs o '[]) GameActions where
  evalIO () _ _ = pure Map.empty

instance
  ( KnownSymbol cmd
  , EvalIO () GetIsDirected dir Bool
  , mkO ~ 'ObjectWrapper o
  , mkA ~ 'ActionWrapper a cmd dir
  , EvalIO () MakeActorAction (ObjAct o a) (ObjectType, ActorAction)
  , EvalIO () MakeGameActions (TraverseActs mkO acts) GameActions
  ) =>
  EvalIO () MakeGameActions (TraverseActs mkO (mkA ': acts)) GameActions where
  evalIO () verb _ = do
    isDirected <- evalIO () GetIsDirected $ Proxy @dir
    let cmd   = symbolVal $ Proxy @cmd

    (oType, actorAct) <- evalIO () MakeActorAction
                                   (Proxy @(ObjAct o a))

    gameActs <- evalIO () verb $ Proxy @(TraverseActs mkO acts)

    let singActorActions :: ActorActions = Map.singleton oType actorAct

    pure $ case Map.lookup cmd gameActs of
      Nothing -> Map.insert cmd (isDirected, singActorActions) gameActs
      Just (_, actorActs) -> let
        actorActs' = Map.insert oType actorAct actorActs
        gameActs'  = Map.insert cmd (isDirected, actorActs') gameActs
        in gameActs'

instance
  EvalIO () GetIsDirected 'True Bool where
  evalIO () _ _ = pure True

instance
  EvalIO () GetIsDirected 'False Bool where
  evalIO () _ _ = pure False


-- Make actors

instance
  ( EvalIO
      (SystemBus, Pos, ObjectInfo)
       MakeActor
       (Objects objects)
       Actor
  ) =>
  EvalIO (SystemBus, FieldObjects)
       MakeActors
       (Objects objects)
       [Actor] where
  evalIO (sysBus, fObjs) _ objs = do
    let evalIOAct p oInfo = evalIO (sysBus, p, oInfo) MakeActor objs
    actors <- mapM (\(p, oInfo) -> evalIOAct p oInfo)
              $ Map.toList fObjs
    pure actors

instance
  EvalIO
    (SystemBus, Pos, ObjectInfo)
     MakeActor
     (Objects '[])
     Actor where
  evalIO (_, pos, oInfo) _ _ =
    error $ "Object not found for " <> show oInfo <> " at " <> show pos

instance
  ( EvalIO () GetObjectInfo o ObjectInfo
  , EvalIO (SystemBus, Pos) MakeActor o Actor
  , EvalIO (SystemBus, Pos, ObjectInfo)
         MakeActor (Objects os) Actor
  ) =>
  EvalIO
    (SystemBus, Pos, ObjectInfo)
     MakeActor
     (Objects ('ObjectWrapper o ': os))
     Actor where
  evalIO payload@(sysBus, pos, oInfo) makeActor _ = do
    oRawInfo <- evalIO () GetObjectInfo $ Proxy @o
    let oTypeEq = oiObjectType oRawInfo == oiObjectType oInfo
    let iconEq  = oiIcon oRawInfo == oiIcon oInfo
    if oTypeEq && iconEq
      then evalIO (sysBus, pos) makeActor $ Proxy @o
      else evalIO payload makeActor $ Proxy @(Objects os)
