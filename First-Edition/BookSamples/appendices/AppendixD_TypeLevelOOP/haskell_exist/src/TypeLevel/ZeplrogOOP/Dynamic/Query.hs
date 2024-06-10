{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeLevel.ZeplrogOOP.Dynamic.Query where

import CPrelude

import TypeLevel.ZeplrogOOP.Dynamic.Model

import qualified Data.Map.Strict as Map
import qualified GHC.Types as GHC
import Unsafe.Coerce (unsafeCoerce)


class QueryValue item where
  queryValueRef :: item -> EssencePath -> IO (IORef Value)


instance QueryValue Property where
  queryValueRef (TagPropRef _) _ = error "queryValueRef: TagPropRef not yet implemented"
  queryValueRef _ [] = error "queryValueRef: Path is empty"
  queryValueRef (Prop _ _ _ fieldsRef _) (ess : esss) = do
    fields <- readIORef fieldsRef
    case Map.lookup ess fields of
      Nothing    -> error $ show $ "queryValueRef: ess not found: " <> ess
      Just field -> queryValueRef field esss

instance QueryValue PropertyOwning where
  queryValueRef (OwnVal valRef) [] = pure valRef
  queryValueRef (OwnVal _) esss = error $ show $ "Path exceeds hierarchy: " <> show esss
  queryValueRef (SharedProp prop) _ =
    error "queryValueRef (SharedProp prop): not yet implemented"
  queryValueRef (OwnDict dictRef) [] =
    error "queryValueRef (OwnDict dictRef): path is empty"
  queryValueRef (OwnDict dictRef) (e:esss) = do
    dict <- readIORef dictRef
    case Map.lookup e dict of
      Nothing -> error $ show $ "Field not found: " <> e
      Just prop -> queryValueRef prop esss
  queryValueRef (OwnProp prop) esss = queryValueRef prop esss


readBoolVal
  :: Property
  -> [Essence]
  -> IO Bool
readBoolVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    BoolValue boolVal -> pure boolVal
    _ -> error "readBoolVal: not a bool value"

readStringVal
  :: Property
  -> [Essence]
  -> IO String
readStringVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    StringValue val -> pure val
    _ -> error "readStringVal: not a string value"

readPathVal
  :: Property
  -> [Essence]
  -> IO [Essence]
readPathVal prop esss = do
  valRef <- queryValueRef prop esss
  val <- readIORef valRef
  case val of
    PathValue val -> pure val
    _ -> error $ "readPathVal: not a path value "
