{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Materialization where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Game.Types
import Minefield.Game.UI

import GHC.TypeLits
import qualified Data.Map as Map


data GetIcon
data GetObjectInfo
data GetObjectType
data MakeActorAction
data MakeGameAction
data MakeGameActions
data MakeActors
data MakeActor
data GetIsDirected

data Objects (a :: [IObject])

data ObjsActs objs acts
data TraverseObjs objs acts
data TraverseActs obj acts
data ObjAct o a

-- Get icon

instance
  ( Eval () GetIcon o Char
  ) =>
  Eval () GetIcon ('ObjectWrapper o) Char where
  eval () vProxy _ = eval () vProxy $ Proxy @o

instance
  Eval () GetIcon (Objects '[]) [Char] where
  eval () _ _ = pure []

instance
  ( Eval () GetIcon o Char
  , Eval () GetIcon (Objects os) [Char]
  ) =>
  Eval () GetIcon (Objects (o ': os)) [Char] where
  eval () vProxy _ = do
    o  <- eval () vProxy $ Proxy @o
    os <- eval () vProxy $ Proxy @(Objects os)
    pure $ o : os

-- Get object info

instance
  ( Eval () GetObjectInfo o ObjectInfo
  ) =>
  Eval () GetObjectInfo ('ObjectWrapper o) ObjectInfo where
  eval () vProxy _ = eval () vProxy $ Proxy @o

instance
  Eval () GetObjectInfo (Objects '[]) [ObjectInfo] where
  eval () _ _ = pure []

instance
  ( Eval () GetObjectInfo o ObjectInfo
  , Eval () GetObjectInfo (Objects os) [ObjectInfo]
  ) =>
  Eval () GetObjectInfo (Objects (o ': os)) [ObjectInfo] where
  eval () vProxy _ = do
    o  <- eval () vProxy $ Proxy @o
    os <- eval () vProxy $ Proxy @(Objects os)
    pure $ o : os

-- Get object type

instance
  ( Eval () GetObjectType o ObjectType
  ) =>
  Eval () GetObjectType ('ObjectWrapper o) ObjectType where
  eval () vProxy _ = eval () vProxy $ Proxy @o

instance
  Eval () GetObjectType (Objects '[]) [ObjectType] where
  eval () _ _ = pure []

instance
  ( Eval () GetObjectType o ObjectType
  , Eval () GetObjectType (Objects os) [ObjectType]
  ) =>
  Eval () GetObjectType (Objects (o ': os)) [ObjectType] where
  eval () vProxy _ = do
    o  <- eval () vProxy $ Proxy @o
    os <- eval () vProxy $ Proxy @(Objects os)
    pure $ o : os

-- Make game action

instance
  ( Eval () MakeGameActions (TraverseObjs os acts) GameActions
  ) =>
  Eval () MakeGameActions (ObjsActs os acts) GameActions where
  eval () proxy _ =
    eval () proxy $ Proxy @(TraverseObjs os acts)

instance
  Eval () MakeGameActions (TraverseObjs '[] acts) GameActions where
  eval () _ _ = pure Map.empty

instance
  ( Eval () MakeGameActions (TraverseObjs os acts) GameActions
  , Eval () MakeGameActions (TraverseActs o acts) GameActions
  ) =>
  Eval () MakeGameActions (TraverseObjs (o ': os) acts) GameActions where
  eval () proxy _ = do
    act1 <- eval () proxy $ Proxy @(TraverseActs o acts)
    act2 <- eval () proxy $ Proxy @(TraverseObjs os acts)
    pure $ Map.union act1 act2

instance
  Eval () MakeGameActions (TraverseActs o '[]) GameActions where
  eval () _ _ = pure Map.empty

instance
  ( KnownSymbol cmd
  , Eval () GetIsDirected dir Bool
  , mkO ~ 'ObjectWrapper o
  , mkA ~ 'ActionWrapper a dir cmd
  , Eval () MakeActorAction (ObjAct o a) (ObjectType, ActorAction)
  , Eval () MakeGameActions (TraverseActs mkO acts) GameActions
  ) =>
  Eval () MakeGameActions (TraverseActs mkO (mkA ': acts)) GameActions where
  eval () proxy _ = do
    isDirected <- eval () (Proxy @GetIsDirected) $ Proxy @dir
    let cmd   = symbolVal $ Proxy @cmd

    (oType, actorAct) <- eval () (Proxy @MakeActorAction)
                              (Proxy @(ObjAct o a))

    printDebugString $ "Cmd: " <> show cmd <> " " <> show oType

    gameActs <- eval () proxy $ Proxy @(TraverseActs mkO acts)

    let singActorActions :: ActorActions = Map.singleton oType actorAct

    pure $ case Map.lookup cmd gameActs of
      Nothing -> Map.insert cmd (isDirected, singActorActions) gameActs
      Just (_, actorActs) -> let
        actorActs' = Map.insert oType actorAct actorActs
        gameActs'  = Map.insert cmd (isDirected, actorActs') gameActs
        in gameActs'

instance
  Eval () GetIsDirected 'True Bool where
  eval () _ _ = pure True

instance
  Eval () GetIsDirected 'False Bool where
  eval () _ _ = pure False


-- Make actors

instance
  ( Eval
      (SystemBus, Pos, ObjectInfo)
       MakeActor
       (Objects objects)
       Actor
  ) =>
  Eval (SystemBus, FieldObjects)
       MakeActors
       (Objects objects)
       [Actor] where
  eval (sysBus, fObjs) _ objs = do
    let makeActor = Proxy @MakeActor
    let evalAct p oInfo = eval (sysBus, p, oInfo) makeActor objs
    actors <- mapM (\(p, oInfo) -> evalAct p oInfo)
              $ Map.toList fObjs
    pure actors

instance
  Eval
    (SystemBus, Pos, ObjectInfo)
     MakeActor
     (Objects '[])
     Actor where
  eval (_, pos, oInfo) _ _ =
    error $ "Object not found for " <> show oInfo <> " at " <> show pos

instance
  ( Eval () GetObjectInfo o ObjectInfo
  , Eval (SystemBus, Pos) MakeActor o Actor
  , Eval (SystemBus, Pos, ObjectInfo)
         MakeActor (Objects os) Actor
  ) =>
  Eval
    (SystemBus, Pos, ObjectInfo)
     MakeActor
     (Objects ('ObjectWrapper o ': os))
     Actor where
  eval payload@(sysBus, pos, oInfo) makeActor _ = do
    oRawInfo <- eval () (Proxy @GetObjectInfo) $ Proxy @o
    let oTypeEq = oiObjectType oRawInfo == oiObjectType oInfo
    let iconEq  = oiIcon oRawInfo == oiIcon oInfo
    if oTypeEq && iconEq
      then eval (sysBus, pos) (Proxy @MakeActor) $ Proxy @o
      else eval payload makeActor $ Proxy @(Objects os)
