{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Context where

import qualified Data.Dynamic as Dyn

class Context ctx where
  getDyn :: ctx -> String -> IO (Maybe Dyn.Dynamic)


mkVal :: Dyn.Typeable valType => valType -> IO (Maybe Dyn.Dynamic)
mkVal = pure . Just . Dyn.toDyn

noVal :: IO (Maybe Dyn.Dynamic)
noVal = pure Nothing
