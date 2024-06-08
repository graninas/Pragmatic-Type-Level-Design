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

import TypeLevel.System.Debug
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

-- Turns a static type-level model into a static value-level model.
-- The resulting model represents a concrete domain with all the entities
-- fully described, all derivings made, and all static properties
-- ready to be used as templates for actual dynamic objects of a game.
-- Static materialization resembles a macro mechanism in languages.

data SrcPropKVs propKVs
data PropOwns propOwns
data Props props
data SS scripts

type ResPropKVs = [PropertyKeyValueVL]
type RSS = [PropertyScriptVL]


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

-- Statically materialize script

instance
  ( SMat () ess EssenceVL
  , SMat () script CustomScriptVL
  ) =>
  SMat () ('PropScript ess script)
          PropertyScriptVL where
  sMat () _ = do
    ess    <- sMat () $ Proxy @ess
    script <- sMat () $ Proxy @script
    pure $ PropScript ess script

-- Statically materialize scripts list

instance
  SMat () (SS '[]) RSS where
  sMat () _ = pure []

instance
  ( SMat () script PropertyScriptVL
  , SMat () (SS scripts) RSS
  ) =>
  SMat () (SS (script ': scripts)) RSS where
  sMat () _ = do
    script  <- sMat () $ Proxy @script
    scripts <- sMat () $ Proxy @(SS scripts)
    pure $ script : scripts

-- -- -- Statically materialize property -- -- --

instance
  ( SMat () tagProp TagPropertyVL
  ) =>
    SMat () ('TagPropRef @'TypeLevel tagProp)
          PropertyVL where
  sMat () _ = do
    tagProp <- sMat () $ Proxy @tagProp
    pure $ TagPropRef tagProp

-- Deriving mechanism for properties.

newtype APropPrepared = APropPrepared PropertyVL

-- Abstract properties will turn into special PropDict properties
--  only once to avoid multiple property preparation.

-- Abstract prop materialization
instance
  ( SMat () group PropertyGroupVL
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () (SS scripts) RSS
  ) =>
  SMat () ('AbstractProp group propKVs scripts)
          APropPrepared where
  sMat () _ = do
    sEnv <- ask
    esss <- readIORef $ seStaticEssencesRef sEnv

    -- N.B. This routine will result in some staticPropertyIds
    --   to be dropped when a singleton property is already present.
    group <- sMat () $ Proxy @group
    let (ess, statPropId) = getComboPropertyId group

    case Map.lookup ess esss of
      Just [(sId, prop)] -> do
        sTraceDebug $ "Prepared abstract property found: "
                    <> show (ess, sId)
        pure $ APropPrepared prop

      Just (_:_:_) ->
        error $ "Multiple prepared properties for abstract prop found, ess: " <> show ess

      _ -> do
        sTraceDebug $ "New abstract property to introduce: "
          <> show ess <> ", sId: " <> show statPropId
        propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
        scripts <- sMat () $ Proxy @(SS scripts)
        let prop = PropDict group propKVs scripts
        addStaticProperty (statPropId, ess, prop)
        sTraceDebug $ show ess <> ": prepared: " <> show statPropId
        pure $ APropPrepared prop

-- Abstract derived prop materialization
instance
  ( SMat () ess EssenceVL
  , SMat () abstractProp APropPrepared
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () (SS scripts) RSS
  ) =>
  SMat () ('AbstractDerivedProp ess abstractProp propKVs scripts)
          APropPrepared where
  sMat () _ = do
    sEnv <- ask
    esss <- readIORef $ seStaticEssencesRef sEnv

    ess <- sMat () $ Proxy @ess

    -- Checking if self is already prepared

    case Map.lookup ess esss of
      Just [(sId, prop)] -> do
        sTraceDebug $ "Prepared abstract derived prop found: "
                    <> show (ess, sId)
        pure $ APropPrepared prop

      Just (_:_:_) ->
        error $ "Multiple prepared properties for abstract derived prop found, ess: " <> show ess

      _ -> do
        statPropId <- getNextStaticPropertyId
        sTraceDebug $ "New abstract derived prop to introduce: "
          <> show ess <> ", sId: " <> show statPropId

        sTraceDebug $ "Preparing parent abstract property for deriving"
        APropPrepared aPropPrepared <- sMat () $ Proxy @abstractProp

        case aPropPrepared of
          PropDict aGroup parentAPropKVs parentAPropScripts -> do
            let aId@(abstractPropEss, abstractPropSId) = getComboPropertyId aGroup
            sTraceDebug $ "Parent abstract property to derive: " <> show aId

            propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
            let propKVs' = mergePropKVs propKVs parentAPropKVs

            scripts <- sMat () $ Proxy @(SS scripts)
            let scripts' = mergeScripts scripts parentAPropScripts

            let prop = PropDict (GroupId ess statPropId) propKVs scripts
            addStaticProperty (statPropId, ess, prop)
            sTraceDebug $ show ess <> ": prepared: " <> show statPropId
            pure $ APropPrepared prop

          _ -> error "Invalid prepared property (not PropDict)."

instance
  ( SMat () ess EssenceVL
  , SMat () abstractProp APropPrepared
  , SMat () (SrcPropKVs propKVs) ResPropKVs
  , SMat () (SS scripts) RSS
  ) =>
  SMat () ('DerivedProp ess abstractProp propKVs scripts)
          PropertyVL where
  sMat () _ = do

    ess <- sMat () $ Proxy @ess
    sTraceDebug $ "Property to derive: " <> show ess

    sTraceDebug $ "Preparing abstract property for deriving"
    APropPrepared aPropPrepared <- sMat () $ Proxy @abstractProp

    case aPropPrepared of
      PropDict aGroup abstractPropKVs abstractPropScripts -> do
        let aId@(abstractPropEss, abstractPropSId) = getComboPropertyId aGroup
        sTraceDebug $ "Abstract property to derive: " <> show aId

        statPropId <- getNextStaticPropertyId

        propKVs <- sMat () $ Proxy @(SrcPropKVs propKVs)
        let propKVs' = mergePropKVs propKVs abstractPropKVs

        scripts <- sMat () $ Proxy @(SS scripts)
        let scripts' = mergeScripts scripts abstractPropScripts

        let prop = PropDict (GroupId ess statPropId) propKVs' scripts'
        addStaticProperty (statPropId, ess, prop)
        sTraceDebug $ show ess <> ": new property is derived: "
                   <> show statPropId
        pure prop

      _ -> error "Invalid prepared property (not PropDict)."

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
  , SMat () (Props props) [PropertyVL]
  ) =>
  SMat () ('PropKeyBag @'TypeLevel ess props)
         PropertyKeyValueVL where
  sMat () _ = do
    ess   <- sMat () $ Proxy @ess
    props <- sMat () $ Proxy @(Props props)
    pure $ PropKeyBag ess props

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
  ( SMat () val ValDefVL
  ) =>
  SMat () ('OwnVal @'TypeLevel val)
          PropertyOwningVL where
  sMat () _ = do
    val <- sMat () $ Proxy @val
    pure $ OwnVal val

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
mergePropKVs
  :: [PropertyKeyValueVL]
  -> [PropertyKeyValueVL]
  -> [PropertyKeyValueVL]
mergePropKVs kvs1 kvs2 = let
  pKVs1 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs1]
  pKVs2 = Map.fromList [ (getEssenceFromKV k, k) | k <- kvs2]
  pKVs3 = Map.union pKVs1 pKVs2
  in Map.elems pKVs3

-- | Merges scripts with preference of the first keys.
mergeScripts
  :: [PropertyScriptVL]
  -> [PropertyScriptVL]
  -> [PropertyScriptVL]
mergeScripts ss1 ss2 = let
  ss1' = Map.fromList [ (ess, ps) | ps@(PropScript ess _) <- ss1]
  ss2' = Map.fromList [ (ess, ps) | ps@(PropScript ess _) <- ss2]
  ss3' = Map.union ss1' ss2'
  in Map.elems ss3'

