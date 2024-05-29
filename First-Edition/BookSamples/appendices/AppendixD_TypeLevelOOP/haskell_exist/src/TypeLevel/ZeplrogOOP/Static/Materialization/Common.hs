{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TypeLevel.ZeplrogOOP.Static.Materialization.Common where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Materialization.Materializer

import TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


-- Helper to materialize the list of essences
data Essences essPath

-- Statically materialize variable def

instance
  ( KnownSymbol str
  ) =>
  SMat () str String where
  sMat () _ = pure $ symbolVal $ Proxy @str


-- -- Statically materialize value

-- instance
--   ( KnownNat intVal
--   ) =>
--   SMat () ('IntValue @'TypeLevel intVal)
--       ValDefVL where
--   sMat () _ = pure
--       $ IntValue
--       $ fromIntegral
--       $ natVal
--       $ Proxy @intVal

-- instance
--   ( SMat () val1 ValDefVL
--   , SMat () val2 ValDefVL
--   ) =>
--   SMat () ('PairValue @'TypeLevel val1 val2) ValDefVL where
--   sMat () _ = do
--     val1 <- sMat () $ Proxy @val1
--     val2 <- sMat () $ Proxy @val2
--     pure $ PairValue val1 val2

-- instance
--   SMat () ('BoolValue @'TypeLevel 'True) ValDefVL where
--   sMat () _ = pure $ BoolValue True

-- instance
--   ( KnownSymbol str
--   ) =>
--   SMat () ('StringValue @'TypeLevel str) ValDefVL where
--   sMat () _ = pure $ StringValue $ symbolVal $ Proxy @str

-- instance
--   SMat () ('BoolValue @'TypeLevel 'False) ValDefVL where
--   sMat () _ = pure $ BoolValue False

-- instance
--   ( SMat () (Essences essPath) [EssenceVL]
--   ) =>
--   SMat () ('PathValue @'TypeLevel essPath)
--          ValDefVL where
--   sMat () _ = do
--     path <- sMat () $ Proxy @(Essences essPath)
--     pure $ PathValue path

-- -- Statically materialize Essence path

-- instance
--   ( KnownSymbol symb
--   ) =>
--   SMat () ('Ess @'TypeLevel symb) EssenceVL where
--   sMat () _ = pure $ Ess $ symbolVal (Proxy @symb)

-- instance
--   SMat () (Essences '[]) [EssenceVL] where
--   sMat () _ = pure []

-- instance
--   ( SMat () ess EssenceVL
--   , SMat () (Essences essPath) [EssenceVL]
--   ) =>
--   SMat () (Essences (ess ': essPath))
--          [EssenceVL] where
--   sMat () _ = do
--     ess     <- sMat () $ Proxy @ess
--     essPath <- sMat () $ Proxy @(Essences essPath)
--     pure $ ess : essPath
