{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeLevel.ZeplrogOOP.QuerySpec where

import CPrelude

import TypeLevel.System.Debug
import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Query
import TypeLevel.ZeplrogOOP.Static.Materialization

import TypeSelector.Granular

import Test.Hspec

import Data.Proxy
import qualified Data.Map.Strict as Map


type EIcon = Ess @TypeLevel "system:icon"
type EIntrinsics = Ess @TypeLevel "category:intrinsics"
type EWall = Ess @TypeLevel "object:wall"

type IconVal icon = StringValue @TypeLevel icon

type TestIconOwning = OwnVal (IconVal "+")
type TestPropKeyVal = PropKeyVal EIcon TestIconOwning

type EAny = Ess @TypeLevel "prop:any"
type AnyProp = AbstractProp (Group EAny) '[] '[]

type TestProp = DerivedProp EIntrinsics AnyProp
  '[ TestPropKeyVal
   ]
  '[]


type Wall = DerivedProp EWall AnyProp
  '[ PropKeyVal EIcon (OwnVal (IconVal "#"))
   ]
  '[]

spec :: Spec
spec = do
  describe "Query spec" $ do
    it "Query string value for own prop" $ do
      sEnv <- makeSEnv DebugDisabled

      owning <- sMat' sEnv () $ Proxy @TestIconOwning
      path   <- sMat' sEnv () $ Proxy @(Essences '[EIcon])

      let mbRes = queryStringValueForOwning path owning
      case mbRes of
        Nothing  -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop key val" $ do
      sEnv <- makeSEnv DebugDisabled

      kv   <- sMat' sEnv () $ Proxy @TestPropKeyVal
      path <- sMat' sEnv () $ Proxy @(Essences '[EIcon])

      let mbRes = queryStringValueForKeyVals path [kv]
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop not relative" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(Essences '[EIntrinsics, EIcon])

      let mbRes = queryStringValue path prop
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"

    it "Query string value for prop relative" $ do
      sEnv <- makeSEnv DebugDisabled

      prop <- sMat' sEnv () $ Proxy @TestProp
      path <- sMat' sEnv () $ Proxy @(Essences '[EIcon])

      let mbRes = queryStringValueRelative path prop
      case mbRes of
        Nothing -> error "String value not found"
        Just val -> val `shouldBe` "+"
