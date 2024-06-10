{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeLevel.ZeplrogOOP.OOPSpec where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Query
import TypeLevel.ZeplrogOOP.Static.Materialization
import qualified TypeLevel.ZeplrogOOP.Dynamic.Model as DMod
import qualified TypeLevel.ZeplrogOOP.Dynamic.Instantiation as DInst
import qualified TypeLevel.ZeplrogOOP.Dynamic.Interaction as Interact
import qualified TypeLevel.ZeplrogOOP.Dynamic.Query as Q
import qualified TypeLevel.ZeplrogOOP.Static.Description as SPrint
import qualified TypeLevel.ZeplrogOOP.Dynamic.Description as DPrint

import TypeLevel.ZeplrogOOP.Testing.Utils
import TypeLevel.System.Debug

import GHC.TypeLits
import TypeSelector.Granular

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import qualified Prelude as P (unlines)



type EColor         = Ess @TypeLevel "color"
type EColorWhite    = Ess @TypeLevel "color:white"
type EColorRed      = Ess @TypeLevel "color:red"
type EColorGreen    = Ess @TypeLevel "color:green"
type EColorBlue     = Ess @TypeLevel "color:blue"
type EColorRef      = Ess @TypeLevel "ref:color"

type EAvailableColors = Ess @TypeLevel "available colors"
type EAbstractLamp    = Ess @TypeLevel "lamp:abstract"
type EIsOn            = Ess @TypeLevel "is on"
type ESwitchScript    = Ess @TypeLevel "script:switch"

type EDaylightLamp    = Ess @TypeLevel "lamp:daylight"
type ETableLamp       = Ess @TypeLevel "lamp:table"
type EBrightness      = Ess @TypeLevel "brightness"
type EBrightnessMax   = Ess @TypeLevel "brightness:max"

type Color      = TagProp (TagGroup EColor)
type ColorWhite = TagProp (TagGroupRoot EColorWhite Color)
type ColorRed   = TagProp (TagGroupRoot EColorRed Color)
type ColorGreen = TagProp (TagGroupRoot EColorGreen Color)
type ColorBlue  = TagProp (TagGroupRoot EColorBlue Color)

type ColorRef = '[ EAvailableColors, EColorWhite ]

type SwitchVar   = BoolVar "switch" 'False
type MismatchVar = StringVar "test" "abc"

type SwitchScript = 'Script @'TypeLevel "inverts the EIsOn switch"
  '[ DeclareVar SwitchVar

  -- Runtime type check works:
  -- readWrite (FromField, ToVar) type mismatch (target is not bool)
  --  , ReadData (FromField 'Proxy '[EIsOn]) (ToVar MismatchVar)

   , ReadData (FromField 'Proxy '[EIsOn]) (ToVar SwitchVar)
   , Invoke NegateF (FromVar SwitchVar) (ToVar SwitchVar)

  -- Compile time type check works:
  -- Couldn't match kind ‘StringTag’ with ‘BoolTag’
  --  , Invoke NegateF (FromVar SwitchVar) (ToVar MismatchVar)

   , WriteData (ToField 'Proxy '[EIsOn]) (FromVar SwitchVar)
   ]


type AbstractLamp = AbstractProp (Group EAbstractLamp)
  '[ PropKeyVal EIsOn (OwnVal (BoolValue False))

   , PropKeyBag EAvailableColors
      '[ TagPropRef ColorWhite
       , TagPropRef ColorRed
       , TagPropRef ColorGreen
       , TagPropRef ColorBlue
       ]

   -- | Current color. Points to a possible color.
   , PropKeyVal EColor (OwnVal (PathValue ColorRef))
   ]
  '[ PropScript ESwitchScript SwitchScript
   ]

type DaylightLamp = DerivedProp EDaylightLamp AbstractLamp
  '[
  ]
  '[
  ]

type TableLamp = DerivedProp ETableLamp AbstractLamp
  '[ PropKeyVal EBrightness (OwnVal (IntValue 50))
   , PropKeyVal EBrightnessMax (OwnVal (IntValue 100))
   ]
  '[]

spec :: Spec
spec =
  describe "Abstract property deriving test" $ do
    it "Static materialization test (daylight lamp)" $ do
      sEnv <- makeSEnv DebugDisabled

      lamp <- sMat' sEnv () $ Proxy @DaylightLamp

      -- let lampDescr = SPrint.describe lamp
      -- putStrLn $ P.unlines lampDescr

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      case lamp of
        PropDict group props scripts -> do
          let (ess, sId) = getComboPropertyId group
          length props `shouldBe` 3
          length statProps `shouldBe` 2
          ess `shouldBe` Ess "lamp:daylight"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    it "Static materialization test (table lamp)" $ do
      sEnv <- makeSEnv DebugDisabled

      lamp <- sMat' sEnv () $ Proxy @TableLamp

      -- let lampDescr = SPrint.describe lamp
      -- putStrLn $ P.unlines lampDescr

      statProps <- readIORef $ seStaticPropertiesRef sEnv
      statEsss  <- readIORef $ seStaticEssencesRef sEnv

      -- print $ "Stat props: " <> show (Map.keys statProps)
      -- print $ "Stat essences: " <> show (Map.keys statEsss)

      case lamp of
        PropDict group props scripts -> do
          let (ess, sId) = getComboPropertyId group
          length props `shouldBe` 5
          length statProps `shouldBe` 2
          ess `shouldBe` Ess "lamp:table"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"

    it "Dynamic instantiation test (daylight lamp)" $ do
      (sEnv, dEnv) <- DInst.makeEnvs DebugDisabled

      lampStat <- sMat' sEnv () $ Proxy @DaylightLamp
      lamp <- DInst.dInstParent dEnv Nothing lampStat

      props <- readIORef $ DInst.dePropertiesRef dEnv
      -- print $ "All props: " <> show (Map.keys props)

      Map.size props `shouldBe` 1

      case lamp of
        DMod.Prop _ _ _ _ _ -> pure ()
        _ -> error "Invalid prop"

    it "Script invoking test (table lamp)" $ do
      (sEnv, dEnv) <- DInst.makeEnvs DebugDisabled

      lampStat <- sMat' sEnv () $ Proxy @TableLamp
      lamp <- DInst.dInstParent dEnv Nothing lampStat

      let scriptEss = toDynEss @ESwitchScript
      let isOnValEsss = [toDynEss @EIsOn]

      descr <- DPrint.describe lamp
      putStrLn $ P.unlines descr

      val1 <- Q.readBoolVal lamp isOnValEsss
      val1 `shouldBe` False

      Interact.invoke scriptEss lamp

      descr <- DPrint.describe lamp
      putStrLn $ P.unlines descr

      val2 <- Q.readBoolVal lamp isOnValEsss
      val2 `shouldBe` True

      props <- readIORef $ DInst.dePropertiesRef dEnv
      Map.size props `shouldBe` 1




