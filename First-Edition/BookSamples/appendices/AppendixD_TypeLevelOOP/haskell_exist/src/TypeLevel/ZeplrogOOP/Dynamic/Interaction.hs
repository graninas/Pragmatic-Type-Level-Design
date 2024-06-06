module TypeLevel.ZeplrogOOP.Dynamic.Interaction where

import CPrelude

import TypeLevel.ZeplrogOOP.Dynamic.Model
import qualified TypeLevel.ZeplrogOOP.Dynamic.Query as Q

import qualified Data.Map as Map

invoke
  :: Essence
  -> Property
  -> IO ()
invoke ess (Prop _ _ _ _ scripts) = do
  case Map.lookup ess scripts of
    Nothing -> error $ "Script not found: " <> show ess
    Just (DynScript act) -> act


