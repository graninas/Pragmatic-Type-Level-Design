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
  queryValue :: item -> EssencePath -> IO (IORef Value)


instance QueryValue Property where
  queryValue (TagPropRef _) _ = error "queryValue: TagPropRef not yet implemented"
  queryValue _ [] = error "queryValue: Path is empty"
  queryValue (Prop _ _ _ fieldsRef _) (ess : esss) = do
    fields <- readIORef fieldsRef
    case Map.lookup ess fields of
      Nothing    -> error $ show $ "queryValue: ess not found: " <> ess
      Just field -> queryValue field esss

instance QueryValue PropertyOwning where
  queryValue (OwnVal valRef) [] = pure valRef
  queryValue (OwnVal _) esss = error $ show $ "Path exceeds hierarchy: " <> show esss
  queryValue _ _ = error "queryValue: not yet implemented"




readBoolVal
  :: Property
  -> [Essence]
  -> IO Bool
readBoolVal prop esss = do
  valRef <- queryValue prop esss
  val <- readIORef valRef
  case val of
    BoolValue boolVal -> pure boolVal
    _ -> error "readBoolVal: not a bool value"
