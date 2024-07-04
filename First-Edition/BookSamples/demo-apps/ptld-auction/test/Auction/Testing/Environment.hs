module Auction.Testing.Environment where

import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Context as Ctx
import qualified TypeLevelDSL.StateContext as StCtx
import qualified TypeLevelDSL.Dyn as Dyn

import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import qualified Data.Aeson as A
import Data.Text (Text)
import Data.IORef
import qualified Data.Text as T
import Test.Hspec


type StringType = String

data MockedFramework = MockedFramework
  { callAPI :: StringType -> IO ()
  , callAPIJson :: A.Value -> IO ()
  }

mkDefaultFramework :: IORef [StringType] -> MockedFramework
mkDefaultFramework apiCallTrackerRef = MockedFramework
  (\req -> modifyIORef' apiCallTrackerRef (req:))
  (\req -> modifyIORef' apiCallTrackerRef (show req:))

data TestData = TestData
  { dynsRef      :: IORef (Map.Map StringType Dyn.Dynamic)
  , subContexts  :: Map.Map StringType TestData
  }

instance Context TestData where
  getDyn TestData {dynsRef} refName = do
    dyns <- readIORef dynsRef
    pure $ Map.lookup refName dyns
  setDyn TestData {dynsRef} refName val = do
    dyns <- readIORef dynsRef
    writeIORef dynsRef $ Map.insert refName val dyns
  getSubContext TestData {subContexts} key = pure $ Map.lookup key subContexts


verifyRef
  :: forall t ctx
   . Dyn.Typeable t
  => Show t
  => Eq t
  => Context ctx
  => ctx
  -> StringType
  -> t
  -> IO ()
verifyRef ctx refName expected = do
  mbRef1 <- getDyn ctx refName
  case (mbRef1, mbRef1 >>= Dyn.fromDynamic) of
    (_, Just v)  -> v `shouldBe` expected
    (Nothing, _) -> fail $ "Ref not found: " <> refName
    (_, Nothing) -> fail $ "Ref not parsed: " <> refName
