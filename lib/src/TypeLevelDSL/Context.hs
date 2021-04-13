{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Context where

import qualified Data.Dynamic as Dyn

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import qualified Data.Map as Map

class Context ctx where
  getDyn :: forall t. ctx -> String -> Proxy t -> IO (Maybe Dyn.Dynamic)
  setDyn :: forall t. ctx -> String -> Dyn.Dynamic -> Proxy t -> IO ()

mkVal :: Dyn.Typeable valType => valType -> IO (Maybe Dyn.Dynamic)
mkVal = pure . Just . Dyn.toDyn

noVal :: IO (Maybe Dyn.Dynamic)
noVal = pure Nothing

toTypeableKey :: forall k. Typeable k => String
toTypeableKey = show $ typeRep (Proxy :: Proxy k)
