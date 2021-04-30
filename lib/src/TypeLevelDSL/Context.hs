{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Context where

import Prelude

import qualified Data.Dynamic as Dyn

import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)
import Data.Text (Text)
import qualified Data.Map as Map

type Key = String
type FieldDescription = String

class Context ctx where
  getDyn        :: ctx -> Key -> IO (Maybe Dyn.Dynamic)
  setDyn        :: ctx -> Key -> Dyn.Dynamic -> IO ()
  getSubContext :: ctx -> Key -> IO (Maybe ctx)


mkVal :: Dyn.Typeable val => val -> Maybe Dyn.Dynamic
mkVal = Just . Dyn.toDyn

mkValIO :: Dyn.Typeable val => val -> IO (Maybe Dyn.Dynamic)
mkValIO = pure . mkVal

noVal :: IO (Maybe Dyn.Dynamic)
noVal = pure Nothing


data ValueError
  = ValueNotFound
  | ValueNotParsed

getValue' :: forall t ctx. (Typeable t, Context ctx) => ctx -> Key -> IO (Either ValueError t)
getValue' ctx key = do
  mbDyn <- getDyn ctx key
  pure $ case (mbDyn, mbDyn >>= Dyn.fromDynamic) of
    (Nothing, _)       -> Left ValueNotFound
    (Just _, Nothing)  -> Left ValueNotParsed
    (Just _, Just val) -> Right val

getValue :: forall t ctx. (Typeable t, Context ctx) => ctx -> Key -> IO (Maybe t)
getValue ctx key = getValue' ctx key >>= \case
  Left _    -> pure Nothing
  Right val -> pure val

getField :: forall t ctx. (Typeable t, Context ctx) => ctx -> Key -> IO (Either String t)
getField ctx key = getValue' ctx key >>= \case
  Left ValueNotFound  -> pure $ Left $ "Field not found: "  <> key
  Left ValueNotParsed -> pure $ Left $ "Field not parsed: " <> key
  Right val           -> pure $ Right val

getField' :: forall t ctx. (Typeable t, Context ctx) => FieldDescription -> ctx -> Key -> IO (Either String t)
getField' descr ctx key = getValue' ctx key >>= \case
  Left ValueNotFound  -> pure $ Left $ "Field " <> descr <> " not found: "  <> key
  Left ValueNotParsed -> pure $ Left $ "Field " <> descr <> " not parsed: " <> key
  Right val           -> pure $ Right val

withSubContext :: forall t ctx. Context ctx => ctx -> Key -> (ctx -> IO (Either String t)) -> IO (Either String t)
withSubContext ctx subCtxKey act = do
  mbSubCtx <- getSubContext ctx subCtxKey
  case mbSubCtx of
    Nothing     -> pure $ Left $ "'" <> subCtxKey <> "' context is not found."
    Just subCtx -> act subCtx
