module TypeLevel.ZeplrogOOP.Dynamic.Scripting where

import CPrelude

import TypeLevel.ZeplrogOOP.Dynamic.Model

import qualified Data.Map as Map

invoke
  :: Essence
  -> Property
  -> IO ()
invoke ess (Prop _ _ _ _ scripts) = do
  case Map.lookup ess scripts of
    Nothing -> error $ "Script not found: " <> show ess
    Just act -> act
