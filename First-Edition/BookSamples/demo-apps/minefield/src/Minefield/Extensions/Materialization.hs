{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Materialization where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface

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
data GetIsDirected

data Objects a

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
  ( Eval () GetObjectInfo o (ObjectType, Char)
  ) =>
  Eval () GetObjectInfo ('ObjectWrapper o) (ObjectType, Char) where
  eval () vProxy _ = eval () vProxy $ Proxy @o

instance
  Eval () GetObjectInfo (Objects '[]) [(ObjectType, Char)] where
  eval () _ _ = pure []

instance
  ( Eval () GetObjectInfo o (ObjectType, Char)
  , Eval () GetObjectInfo (Objects os) [(ObjectType, Char)]
  ) =>
  Eval () GetObjectInfo (Objects (o ': os)) [(ObjectType, Char)] where
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
  Eval (SystemBus, FieldObjects)
       MakeActors
       (Objects (player ': emptyCell ': objects))
       [Actor] where
  eval _ _ _ = pure []
