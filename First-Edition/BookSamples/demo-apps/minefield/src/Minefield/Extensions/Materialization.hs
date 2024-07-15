{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Materialization where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Interface

import Minefield.Game.Types

import GHC.TypeLits


data GetIcon
data MakeGameAction

data Objects a

data ObjsActs objs acts
data TraverseObjs objs acts
data TraverseActs obj acts
data ObjAct o a


-- Get icon

instance
  ( Eval GetIcon o Char
  ) =>
  Eval GetIcon ('ObjectWrapper o) Char where
  eval vProxy _ = eval vProxy $ Proxy @o

instance
  Eval GetIcon (Objects '[]) [Char] where
  eval _ _ = pure []

instance
  ( Eval GetIcon o Char
  , Eval GetIcon (Objects os) [Char]
  ) =>
  Eval GetIcon (Objects (o ': os)) [Char] where
  eval vProxy _ = do
    o  <- eval vProxy $ Proxy @o
    os <- eval vProxy $ Proxy @(Objects os)
    pure $ o : os

-- Make game action

instance
  ( Eval MakeGameAction (TraverseObjs os acts) GameAction
  ) =>
  Eval MakeGameAction (ObjsActs os acts) GameAction where
  eval proxy _ =
    eval proxy $ Proxy @(TraverseObjs os acts)

instance
  Eval MakeGameAction (TraverseObjs '[] acts) GameAction where
  eval _ _ = pure $ \_ _ -> pure ()

instance
  ( Eval MakeGameAction (TraverseObjs os acts) GameAction
  , Eval MakeGameAction (TraverseActs o acts) GameAction
  ) =>
  Eval MakeGameAction (TraverseObjs (o ': os) acts) GameAction where
  eval proxy _ = do
    act1 <- eval proxy $ Proxy @(TraverseActs o acts)
    act2 <- eval proxy $ Proxy @(TraverseObjs os acts)
    pure $ \sysBus pos -> do
      act1 sysBus pos
      act2 sysBus pos

instance
  Eval MakeGameAction (TraverseActs o '[]) GameAction where
  eval _ _ = pure $ \_ _ -> pure ()

instance
  ( mkO ~ 'ObjectWrapper o
  , mkA ~ 'ActionWrapper a dir cmd
  , Eval MakeGameAction (ObjAct o a) GameAction
  , Eval MakeGameAction (TraverseActs mkO acts) GameAction
  ) =>
  Eval MakeGameAction (TraverseActs mkO (mkA ': acts)) GameAction where
  eval proxy _ = do
    act1 <- eval proxy $ Proxy @(TraverseActs mkO acts)
    act2 <- eval (Proxy @MakeGameAction) $ Proxy @(ObjAct o a)
    pure $ \sysBus pos -> do
      act1 sysBus pos
      act2 sysBus pos
