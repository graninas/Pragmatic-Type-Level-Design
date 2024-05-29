{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TypeLevel.ZeplrogOOP.Static.Materialization.Property where

import CPrelude

import TypeLevel.ZeplrogOOP.System.Debug
import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Materialization.Materializer
import TypeLevel.ZeplrogOOP.Static.Materialization.Common
import TypeLevel.ZeplrogOOP.Static.Materialization.Script
import TypeLevel.ZeplrogOOP.Static.Query

import TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Static property materialization --------------

data SrcPropKVs propKVs
data PropOwns propOwns
data Props props

type ResPropKVs = [PropertyKeyValueVL]




withSingletonProperty
  :: SMat () group PropertyGroupVL
  => Proxy group
  -> (PropertyGroupVL -> SMaterializer PropertyVL)
  -> SMaterializer PropertyVL
withSingletonProperty groupProxy matPropF = do
  sEnv <- ask

  -- N.B. This routine will result in some staticPropertyIds
  --   to be dropped when a singleton property is already present.
  group <- sMat () groupProxy
  let (ess, statPropId) = getComboPropertyId group

  esss <- readTVarIO $ seStaticEssencesVar sEnv

  case Map.lookup ess esss of
    Just [(sId, prop)] -> do
      sTraceDebug $ "Singleton static property found: "
                  <> show (ess, sId)
      pure prop

    Just (_:_:_) ->
      error $ "Multiple properties for singleton prop found, ess: " <> show ess

    _ -> do
      sTraceDebug $ "New singleton static property to introduce: "
        <> show ess <> ", sId: " <> show statPropId
      prop <- matPropF group
      addStaticProperty (statPropId, ess, prop)
      sTraceDebug $ show ess <> ": created: " <> show statPropId
      pure prop


withProperty
  :: SMat () group PropertyGroupVL
  => Proxy group
  -> (PropertyGroupVL -> SMaterializer PropertyVL)
  -> SMaterializer PropertyVL
withProperty groupProxy matPropF = do
  sEnv <- ask

  group <- sMat () groupProxy
  let (ess, statPropId) = getComboPropertyId group

  esss <- readTVarIO $ seStaticEssencesVar sEnv
  sTraceDebug $ "Static property to introduce: " <> show ess
  prop <- matPropF group
  addStaticProperty (statPropId, ess, prop)
  sTraceDebug $ show ess <> ": created: " <> show statPropId
  pure prop

-- Statically materialize property group

instance
  ( SMat () ess EssenceVL
  ) =>
  SMat () ('Group ess)
          PropertyGroupVL where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess
    sId <- getNextStaticPropertyId
    pure $ GroupId ess sId

instance
  ( SMat () ess EssenceVL
  , SMat () prop PropertyVL
  ) =>
  SMat () ('GroupRoot ess prop)
          PropertyGroupVL where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess
    sId <- getNextStaticPropertyId
    prop <- sMat () $ Proxy @prop
    pure $ GroupRootId ess sId prop

-- Statically materialize property

instance
  ( SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () group PropertyGroupVL
  ) =>
  SMat () ('PropDict @'TypeLevel group propKVs)
          PropertyVL where
  sMat () _ = withProperty (Proxy @group) $ \group -> do
    propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
    pure $ PropDict group propKVs

instance
  ( SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () group PropertyGroupVL
  ) =>
  SMat () ('AbstractProp @'TypeLevel group propKVs)
          PropertyVL where
  sMat () _ = withSingletonProperty (Proxy @group) $ \group -> do
    propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
    pure $ AbstractProp group propKVs

-- | Deriving mechanism for abstract properties.
instance
  ( SMat () ess EssenceVL
  , SMat () abstractProp PropertyVL
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat () ('DerivedProp @'TypeLevel ess abstractProp propKVs)
          PropertyVL where
  sMat () _ = do
    ess <- sMat () $ Proxy @ess

    sTraceDebug $ "Deriving property: " <> show ess

    abstractProp <- sMat () $ Proxy @abstractProp

    case abstractProp of
      AbstractProp group abstractPropKVs -> do
        let aId@(abstractPropEss, abstractPropSId) = getComboPropertyId group
        sTraceDebug $ "Abstract property to derive: " <> show aId

        statPropId <- getNextStaticPropertyId

        propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
        let propKVs' = mergePropKVs propKVs abstractPropKVs

        let prop = PropDict (GroupRootId ess statPropId abstractProp) propKVs'
        addStaticProperty (statPropId, ess, prop)
        sTraceDebug $ show ess <> ": new property is derived: "
                   <> show statPropId
        pure prop

      _ -> error "non-abstract props are not supported yet."

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('StaticPropRef @'TypeLevel prop)
          PropertyVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    pure $ StaticPropRef prop

instance
  ( SMat () group PropertyGroupVL
  ) =>
  SMat () ('StaticProp @'TypeLevel group)
          PropertyVL where
  sMat () _ =
    withSingletonProperty (Proxy @group) $ \group -> do
    -- pure $ StaticPropRef $ StaticProp group    --  ???
      pure $ StaticProp group

-- Statically materialize property key value list

instance
  SMat () (SrcPropKVs '[]) ResPropKVs where
  sMat () _ = pure []

instance
  ( SMat () propKV PropertyKeyValueVL
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  ) =>
  SMat () (SrcPropKVs (propKV ': propKVs)) ResPropKVs where
  sMat () _ = do
    propKV  <- sMat () $ Proxy @propKV
    propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
    pure $ propKV : propKVs

-- Statically materialize property key value

instance
  ( SMat () ess EssenceVL
  , SMat () propOwn PropertyOwningVL
  ) =>
  SMat () ('PropKeyVal @'TypeLevel ess propOwn)
         PropertyKeyValueVL where
  sMat () _ = do
    ess     <- sMat () $ Proxy @ess
    propOwn <- sMat () $ Proxy @propOwn
    pure $ PropKeyVal ess propOwn

instance
  ( SMat () ess EssenceVL
  , SMat () (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat () ('PropKeyBag @'TypeLevel ess propOwns)
         PropertyKeyValueVL where
  sMat () _ = do
    ess      <- sMat () $ Proxy @ess
    propOwns <- sMat () $ Proxy @(PropOwns propOwns)
    pure $ PropKeyBag ess propOwns

-- Statically materialize property ownings

instance
  SMat () (PropOwns '[]) [PropertyOwningVL] where
  sMat () _ = pure []

instance
  ( SMat () propOwn PropertyOwningVL
  , SMat () (PropOwns propOwns) [PropertyOwningVL]
  ) =>
  SMat () (PropOwns (propOwn ': propOwns))
         [PropertyOwningVL] where
  sMat () _ = do
    propOwn  <- sMat () $ Proxy @propOwn
    propOwns <- sMat () $ Proxy @(PropOwns propOwns)
    pure $ propOwn : propOwns

-- Statically materialize property owning

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('OwnProp @'TypeLevel prop)
          PropertyOwningVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    pure $ OwnProp prop

instance
  ( SMat () prop PropertyVL
  ) =>
  SMat () ('SharedProp @'TypeLevel prop)
          PropertyOwningVL where
  sMat () _ = do
    prop <- sMat () $ Proxy @prop
    pure $ SharedProp prop

-- Statically materialize list of properties

instance
  SMat () (Props '[]) [PropertyVL] where
  sMat () _ = pure []

instance
  ( SMat () prop PropertyVL
  , SMat () (Props props) [PropertyVL]
  ) =>
  SMat () (Props (prop ': props)) [PropertyVL] where
  sMat () _ = do
    prop  <- sMat () $ Proxy @prop
    props <- sMat () $ Proxy @(Props props)
    pure $ prop : props



-- | Merges props with preference of the first keys.
-- Does not merge internal props.
mergePropKVs :: [PropertyKeyValueVL] -> [PropertyKeyValueVL] -> [PropertyKeyValueVL]
mergePropKVs kvs1 kvs2 = let
  pKVs1 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs1]
  pKVs2 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs2]
  pKVs3 = Map.union pKVs1 pKVs2
  in Map.elems pKVs3

