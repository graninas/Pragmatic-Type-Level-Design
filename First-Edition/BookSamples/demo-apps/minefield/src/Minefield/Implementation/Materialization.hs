{-# LANGUAGE UndecidableInstances #-}

module Minefield.Implementation.Materialization where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import GHC.TypeLits
import qualified Data.Map as Map


data MakeActors           = MakeActors
data MakeActor            = MakeActor
data GetIsDirected        = GetIsDirected
data MaterializeField     = MaterializeField
data MaterializeFieldImpl = MaterializeFieldImpl

data Objects (a :: [IObjectTemplate])

-- data ObjsActs objs acts
-- data TraverseObjs objs acts
-- data TraverseActs obj acts
-- data ObjAct o a

-- Get icon

instance
  ( EvalIO () GetIcon o Char
  ) =>
  EvalIO () GetIcon ('ObjectTemplateWrapper o) Char where
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
  EvalIO () GetObjectInfo ('ObjectTemplateWrapper o) ObjectInfo where
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
  EvalIO () GetObjectType ('ObjectTemplateWrapper o) ObjectType where
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

data MakeActorAction      = MakeActorAction
data MakeGameAction       = MakeGameAction
data MakeGameActions      = MakeGameActions

data ActsObjs acts objs
data GAct act objs
data ObjectVerb obj act

instance
  EvalIO () GetIsDirected 'True Bool where
  evalIO () _ _ = pure True

instance
  EvalIO () GetIsDirected 'False Bool where
  evalIO () _ _ = pure False

instance
  EvalIO () MakeGameActions (ActsObjs '[] objs) GameActions where
  evalIO () _ _ = pure Map.empty

instance
  EvalIO () MakeGameActions (ActsObjs acts '[]) GameActions where
  evalIO () _ _ = pure Map.empty

instance
  ( EvalIO () MakeGameAction (GAct act objs)
              (TextCommand, IsDirected, ActorActions)
  , EvalIO () MakeGameActions (ActsObjs acts objs) GameActions
  ) =>
  EvalIO () MakeGameActions (ActsObjs (act ': acts) objs) GameActions where
  evalIO () verb _ = do
    (cmd, isDirected, aActs) <- evalIO () MakeGameAction $ Proxy @(GAct act objs)
    gActs <- evalIO () MakeGameActions $ Proxy @(ActsObjs acts objs)

    when (Map.member cmd gActs) $ error $ show $ "Duplicate cmd: " <> cmd

    pure $ Map.insert cmd (isDirected, aActs) gActs


instance
  ( KnownSymbol cmd
  , EvalIO () GetIsDirected dir IsDirected
  ) =>
  EvalIO () MakeGameAction (GAct ('ActionWrapper a cmd dir) '[])
            (TextCommand, IsDirected, ActorActions) where
  evalIO () _ _ = do
    isDirected <- evalIO () GetIsDirected $ Proxy @dir
    let cmd = symbolVal $ Proxy @cmd
    pure (cmd, isDirected, Map.empty)

instance
  ( EvalIO () MakeGameAction (GAct a os) (TextCommand, IsDirected, ActorActions)
  , EvalIO () MakeActorAction (ObjectVerb o a) (ObjectType, ActorAction)
  ) =>
  EvalIO () MakeGameAction (GAct a (o ': os)) (TextCommand, IsDirected, ActorActions) where
  evalIO () _ _ = do
    (cmd, isDirected, acts) <- evalIO () MakeGameAction $ Proxy @(GAct a os)

    -- extension point
    (oType, act) <- evalIO () MakeActorAction $ Proxy @(ObjectVerb o a)

    when (Map.member oType acts) $ error $ show $ "Duplicate oType: " <> oType

    pure (cmd, isDirected, Map.insert oType act acts)

-- extension point entry
instance
  ( EvalIO () MakeActorAction (ObjectVerb o a) (ObjectType, ActorAction)
  ) =>
  EvalIO () MakeActorAction
    (ObjectVerb ('ObjectTemplateWrapper o)
                ('ActionWrapper a cmd dir))
    (ObjectType, ActorAction) where
  evalIO () _ _ =
    evalIO () MakeActorAction $ Proxy @(ObjectVerb o a)

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
     (Objects ('ObjectTemplateWrapper o ': os))
     Actor where
  evalIO payload@(sysBus, pos, oInfo) makeActor _ = do
    oRawInfo <- evalIO () GetObjectInfo $ Proxy @o
    let oTypeEq = oiObjectType oRawInfo == oiObjectType oInfo
    let iconEq  = (fst (oiIcons oRawInfo)) == (fst (oiIcons oInfo))
    if oTypeEq && iconEq
      then evalIO (sysBus, pos) makeActor $ Proxy @o
      else evalIO payload makeActor $ Proxy @(Objects os)


-- Materializing the field

type ObjInfoMap = Map.Map Icon ObjectInfo

instance
  ( EvalIO (Int, ObjInfoMap) MaterializeFieldImpl rs FieldObjects
  ) =>
  EvalIO (Int, ObjInfoMap) MaterializeField rs FieldObjects where
  evalIO p _ _ = do
    field <- evalIO p MaterializeFieldImpl $ Proxy @rs
    let field' = Map.mapKeys (\(x, y) -> (x, abs y)) field
    pure field'


instance
  EvalIO (Int, ObjInfoMap) MaterializeFieldImpl '[] FieldObjects where
  evalIO _ _ _ = pure Map.empty

instance
  ( KnownSymbol r
  , EvalIO (Int, ObjInfoMap) MaterializeFieldImpl rs FieldObjects
  ) =>
  EvalIO (Int, ObjInfoMap) MaterializeFieldImpl (r:rs) FieldObjects where
  evalIO (rIdx, oInfos) _ _ = do
    field <- evalIO (rIdx - 1, oInfos) MaterializeFieldImpl $ Proxy @rs
    let row = symbolVal $ Proxy @r
    let field' = foldr (insertObj rIdx oInfos) field $ zip [0..] row
    pure field'

insertObj rIdx oInfos (cIdx, icon) field =
  case Map.lookup icon oInfos of
    Nothing -> error $ "Icon not found: " <> show icon
    Just oi -> Map.insert (cIdx, rIdx) oi field
