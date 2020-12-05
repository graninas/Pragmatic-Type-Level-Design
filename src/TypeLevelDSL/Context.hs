{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Context where

import qualified Data.Dynamic as Dyn

class GetVal ctx valName valType | ctx valName -> valType where
  getVal :: ctx -> valType

class Context ctx where
  getDyn :: ctx -> String -> IO (Maybe Dyn.Dynamic)
  getVal' :: forall valType . ctx -> String -> IO (Maybe valType)


getDyn'
  :: forall ctx valType
   . Context ctx
  => Dyn.Typeable valType
  => ctx
  -> String
  -> IO (Maybe Dyn.Dynamic)
getDyn' ctx valName = do
  (mbV :: Maybe valType) <- getVal' ctx valName
  pure $ mbV >>= Just . Dyn.toDyn
