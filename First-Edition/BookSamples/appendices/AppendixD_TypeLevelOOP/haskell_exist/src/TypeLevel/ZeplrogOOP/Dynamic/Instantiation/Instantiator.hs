{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeLevel.ZeplrogOOP.Dynamic.Instantiation.Instantiator where

import CPrelude

import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod
import qualified TypeLevel.ZeplrogOOP.Static.Query as SQuery
import qualified TypeLevel.ZeplrogOOP.Static.Materialization as SMat
import TypeLevel.ZeplrogOOP.Dynamic.Model

import TypeLevel.System.Debug

import qualified Data.Map.Strict as Map


---------- Dynamic instantiation interface ------------------

type DynamicProperties = Map.Map PropertyId (Essence, Property)
type SharedProperties  = Map.Map SMod.StaticPropertyId (Essence, Property)
type DynamicEssences   = Map.Map Essence [(PropertyId, Property)]

data DEnv = DEnv
  { deSEnv :: SMat.SEnv
    -- ^ Static environment
  , dePropertyIdVar       :: TVar PropertyId
    -- ^ PropId counter
  , dePropertiesVar       :: TVar DynamicProperties
    -- ^ List of all dynamic props
  , deSharedPropertiesVar :: TVar SharedProperties
    -- ^ List of shared props
  , deEssencesVar         :: TVar DynamicEssences
    -- ^ List of all dynamic props
  }

type DInstantiator a = ReaderT DEnv IO a

type Shared = Bool

-- | Instantiation type class.
class DInst payload a b | payload a -> b where
  dInst :: Shared -> payload -> a -> DInstantiator b

runDInstantiator :: DEnv -> DInstantiator a -> IO a
runDInstantiator dEnv m = runReaderT m dEnv

dInst' :: DInst payload a b => DEnv -> payload -> a -> IO b
dInst' dEnv p itVL = runDInstantiator dEnv $ dInst False p itVL

dInstParent
  :: DInst (Maybe PropertyId) SMod.PropertyVL (Essence, Property)
  => DEnv -> Maybe PropertyId -> SMod.PropertyVL -> IO Property
dInstParent dEnv mbParentId pVL = do
  (_ :: Essence, prop) <- runDInstantiator dEnv $ dInst False mbParentId pVL
  pure prop

fullInst
  :: SMat.SMat payload itTL itVL
  => DInst payload itVL res
  => DEnv
  -> payload
  -> Proxy itTL
  -> IO res
fullInst dEnv p proxy = do
  itVL <- SMat.sMat' (deSEnv dEnv) p proxy
  dInst' dEnv p itVL

makeDEnv :: SMat.SEnv -> IO DEnv
makeDEnv sEnv = DEnv
  <$> pure sEnv
  <*> newTVarIO (PropertyId 0)
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty
  <*> newTVarIO Map.empty

makeEnvs :: DebugMode -> IO (SMat.SEnv, DEnv)
makeEnvs dbg = do
  sEnv <- SMat.makeSEnv dbg
  dEnv <- makeDEnv sEnv
  pure (sEnv, dEnv)

---------- Utils -----------------

getNextPropertyId'
  :: TVar PropertyId
  -> DInstantiator PropertyId
getNextPropertyId' propIdVar = atomically $ do
  PropertyId pId <- readTVar propIdVar
  writeTVar propIdVar $ PropertyId $ pId + 1
  pure $ PropertyId pId

getNextPropertyId :: DInstantiator PropertyId
getNextPropertyId = do
  propIdVar <- asks dePropertyIdVar
  getNextPropertyId' propIdVar

getPropertyEssence :: PropertyId -> DInstantiator Essence
getPropertyEssence propId = do
  propsVar <- asks dePropertiesVar
  props    <- readTVarIO propsVar
  case Map.lookup propId props of
    Nothing -> error
      $ "Property essence not found for pId: " <> show propId
    Just (ess, _) -> pure ess

dTraceDebug :: DInstantiator String -> DInstantiator ()
dTraceDebug mMsg = do
  dbg <- asks $ SMat.seDebugMode . deSEnv
  when (dbg == DebugEnabled) $ do
    msg <- mMsg
    trace msg $ pure ()


-- | Finalizes creation of property.
-- Registers the property in maps.
spawnProperty
  :: DInstantiator (Essence, Property)
  -> DInstantiator (Essence, Property)
spawnProperty propMat = do
  (ess, prop) <- propMat
  let propId = pPropertyId prop

  propsVar  <- asks dePropertiesVar
  esssVar   <- asks deEssencesVar

  pId <- atomically $ do
    props  <- readTVar propsVar
    esss   <- readTVar esssVar

    let props'  = Map.insert propId (ess, prop) props
    writeTVar propsVar props'

    case Map.lookup ess esss of
      Nothing -> writeTVar esssVar
        $ Map.insert ess [(propId, prop)] esss
      Just ps -> writeTVar esssVar
        $ Map.insert ess ((propId, prop) : ps) esss


  dTraceDebug $ do
    pure $ "Dyn property created: "
      <> " "
      <> show ess
      <> ", pId: "
      <> show pId

  pure (ess, prop)

withSMaterializer
  :: SMat.SMaterializer a
  -> DInstantiator a
withSMaterializer sMaterializer = do
  sEnv <- asks deSEnv
  liftIO $ SMat.runSMaterializer sEnv sMaterializer
