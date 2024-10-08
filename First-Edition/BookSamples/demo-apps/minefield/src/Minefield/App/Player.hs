module Minefield.App.Player where

import CPrelude

import qualified Prelude as P

import Minefield.Core.Types
import Minefield.Core.System.Types
import Minefield.Core.Commons
import Minefield.Core.Defaults
import Minefield.App.Runtime

import qualified Data.Map as Map


performPlayerCommand
  :: SystemBus
  -> PlayerPos
  -> PlayerCommand
  -> GameIO ()
performPlayerCommand sysBus pos (PlayerCommand mbDir actorActions) = do
  let acts = Map.toList actorActions
  case mbDir of
    Nothing  -> mapM_ (performActorAction pos) acts
    Just dir -> mapM_ (performActorAction (movePos pos dir)) acts
  where
    performActorAction pos' (oType, act) = act sysBus oType pos'

parsePlayerCommand
  :: String
  -> GameActions
  -> GameIO (Either String PlayerCommand)
parsePlayerCommand line actions = do

  let ws = P.words line

  case ws of
    [] -> pure $ Left $ "Empty command: " <> line
    (cmd : rest) -> do
      case Map.lookup cmd actions of
        Nothing                -> pure $ Left $ "Command not found: " <> cmd
        Just (isDirected, act) -> do
          case (isDirected, rest) of
            (False, _)        -> pure $ Right $ PlayerCommand Nothing act
            (True, [])        -> pure $ Left $ "Directed command lacks direction: " <> line
            (True, (arg : _)) ->
              case readMaybe arg of
                Nothing  -> pure $ Left $ "Direction parse error: " <> arg
                Just dir -> pure $ Right $ PlayerCommand (Just dir) act
