{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Context where

import qualified Data.Dynamic as Dyn

class GetVal ctx valName valType | ctx valName -> valType where
  getVal :: ctx -> valType

class Context ctx where
  getDyn :: ctx -> String -> IO (Maybe Dyn.Dynamic)
