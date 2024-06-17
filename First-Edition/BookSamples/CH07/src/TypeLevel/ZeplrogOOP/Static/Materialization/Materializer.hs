{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TypeLevel.ZeplrogOOP.Static.Materialization.Materializer where

import CPrelude

import TypeLevel.System.Debug
import TypeLevel.ZeplrogOOP.Static.Model

import TypeSelector.Granular
import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as Map


---------- Interface ------------------

type StaticProperties = Map.Map StaticPropertyId (EssenceVL, PropertyVL)
type StaticEssences   = Map.Map EssenceVL [(StaticPropertyId, PropertyVL)]

data SEnv = SEnv
  { seDebugMode           :: DebugMode
  , seStaticPropertyIdRef :: IORef StaticPropertyId
  , seStaticPropertiesRef :: IORef StaticProperties
  , seStaticEssencesRef   :: IORef StaticEssences
  }

type SMaterializer a = ReaderT SEnv IO a

-- | Materialization type class.
class SMat payload a b | payload a -> b where
  sMat :: payload -> Proxy a -> SMaterializer b

-- | Special payload to make specific instances when materializing props.
data Instantiate
  = InstantiateValue
      [EssenceVL]        -- ^ Possible path to this value
      ValDefVL           -- ^ Value to instantiate

-- | Run materializer with an environment.
runSMaterializer :: SEnv -> SMaterializer a -> IO a
runSMaterializer sEnv m = do
  when (seDebugMode sEnv == DebugEnabled) $ trace "\n" $ pure ()
  runReaderT m sEnv

-- | Materialize a type.
sMat' :: SMat payload a b => SEnv -> payload -> Proxy a -> IO b
sMat' sEnv p proxy = runSMaterializer sEnv $ sMat p proxy

-- | Create the environment.
makeSEnv :: DebugMode -> IO SEnv
makeSEnv dbg = SEnv
  <$> pure dbg
  <*> newIORef (StaticPropertyId 0)
  <*> newIORef Map.empty
  <*> newIORef Map.empty

----- Utils ---------------

getStaticProperty
  :: StaticPropertyId
  -> SMaterializer (EssenceVL, PropertyVL)
getStaticProperty statPropId = do
  statPropsRef <- asks seStaticPropertiesRef
  statProps    <- readIORef statPropsRef
  case Map.lookup statPropId statProps of
    Nothing -> error
      $ "Static property " <> show statPropId <> " not found."
    Just prop -> pure prop

getNextStaticPropertyId'
  :: IORef StaticPropertyId
  -> SMaterializer StaticPropertyId
getNextStaticPropertyId' statPropIdRef = do
  StaticPropertyId pId <- readIORef statPropIdRef
  writeIORef statPropIdRef $ StaticPropertyId $ pId + 1
  pure $ StaticPropertyId pId

getNextStaticPropertyId :: SMaterializer StaticPropertyId
getNextStaticPropertyId = do
  statPropIdRef <- asks seStaticPropertyIdRef
  getNextStaticPropertyId' statPropIdRef


addStaticProperty
  :: (StaticPropertyId, EssenceVL, PropertyVL)
  -> SMaterializer ()
addStaticProperty (statPropId, ess, prop) = do
  statPropsRef    <- asks seStaticPropertiesRef
  statEssencesRef <- asks seStaticEssencesRef
  do
    props <- readIORef statPropsRef
    esss  <- readIORef statEssencesRef

    writeIORef statPropsRef
      $ Map.insert statPropId (ess, prop) props

    case Map.lookup ess esss of
      Nothing -> writeIORef statEssencesRef
        $ Map.insert ess [(statPropId, prop)] esss
      Just ps -> writeIORef statEssencesRef
        $ Map.insert ess ((statPropId, prop) : ps) esss


sTraceDebug :: String -> SMaterializer ()
sTraceDebug msg = do
  dbg <- asks seDebugMode
  when (dbg == DebugEnabled) $ trace msg $ pure ()


