{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Property where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod
import qualified TypeLevel.ZeplrogOOP.Static.Query as SQuery
import qualified TypeLevel.ZeplrogOOP.Static.Materialization.Materializer as SMat
import TypeLevel.ZeplrogOOP.Dynamic.Model
import TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Instantiator
import TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Common
import qualified TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Script as Script

import Data.Proxy
import qualified Data.Map.Strict as Map


-- | Verifies if the shared property already exists.
-- Call spawnProperty to finalize creation of the prop.
withShared
  :: Bool
  -> SMod.PropertyGroupVL
  -> DInstantiator (Essence, Property)
  -> DInstantiator (Essence, Property)
withShared shared group matProp = do
  sharedVar   <- asks deSharedPropertiesVar
  sharedProps <- readTVarIO sharedVar
  (ess, sId) <- dInst False () group
  case (shared, Map.lookup sId sharedProps) of
    (True, Nothing) -> do
      (_, prop) <- matProp
      atomically $ writeTVar sharedVar
                 $ Map.insert sId (ess, prop) sharedProps
      pure (ess, prop)
    (True,  Just (_, prop)) -> pure (ess, prop)
    (False, Just (pId, _)) -> error
      $ "Not shared dynamic property found in shared props: "
        <> show ess <> ", pId: " <> show pId
    (False, Nothing) -> do
      (_, prop) <- matProp
      pure (ess, prop)

---------- Instantiation --------------

-- Instantiate property

type MbParentId = Maybe PropertyId



instance
  DInst MbParentId SMod.PropertyVL (Essence, Property) where
  dInst shared _ (SMod.TagPropRef tagProp) = do
    ess <- dInst False () $ SQuery.getTagPropEss tagProp
    pure (ess, TagPropRef tagProp)

  dInst shared mbParentId (SMod.PropDict group propKVs scripts) =
    spawnProperty $ withShared shared group $ do
      let (sEss, sId) = SQuery.getComboPropertyId group

      ess         <- dInst False () sEss
      propId      <- getNextPropertyId
      props       <- mapM (dInst False (Just propId)) propKVs
      propBagsVar <- newTVarIO $ Map.fromList props

      -- Prop without scripts
      let tmpProp = Prop propId mbParentId sId propBagsVar Map.empty

      -- scripts' <- mapM (makeScript tmpProp) $ Map.toList scripts
      -- pure (ess, tmpProp {pScripts = Map.fromList scripts'})

      pure (ess, tmpProp)

makeScript
  :: Property
  -> (SMod.EssenceVL, SMod.PropertyScriptVL)
  -> DInstantiator (Essence, IO ())
makeScript prop (statEss, pS) = do
  DEnv sEnv _ _ _ _ <- ask

  ess <- dInst False () statEss
  -- res <- liftIO $ matScript sEnv proxy

  pure (ess, pure ())


instance
  DInst MbParentId SMod.PropertyOwningVL PropertyOwning where

  dInst _ _ (SMod.OwnVal valDef) = do
    val    <- dInst False () valDef
    valVar <- newTVarIO val
    pure (OwnVal valVar)

  dInst _ mbParentId (SMod.OwnProp statProp) = do
    (_, prop) <- dInst False mbParentId statProp
    pure (OwnProp prop)

  dInst _ mbParentId (SMod.SharedProp statProp) = do
    (_, prop) <- dInst True mbParentId statProp
    -- FIXME: there is a bug here. `pPropertyId` is not working
    --  for all cases
    pure (SharedProp $ DynamicPropRef $ pPropertyId prop)

instance
  DInst MbParentId SMod.PropertyKeyValueVL (Essence, PropertyOwning) where

  dInst _ mbParentId (SMod.PropKeyVal keyEss statOwning) = do
    keyEss <- dInst False () keyEss
    owning <- dInst False mbParentId statOwning
    pure (keyEss, owning)

  dInst _ mbParentId (SMod.PropKeyBag keyEss props) = do
    keyEss   <- dInst False () keyEss
    props    <- mapM (dInst False mbParentId) props
    propsVar <- newTVarIO $ Map.fromList props
    pure (keyEss, OwnDict propsVar)
