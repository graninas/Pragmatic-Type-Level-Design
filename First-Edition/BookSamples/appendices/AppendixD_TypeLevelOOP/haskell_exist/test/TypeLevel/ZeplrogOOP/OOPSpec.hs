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

import TypeLevel.System.Debug

import GHC.TypeLits
import TypeSelector.Granular

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import           Data.IORef
import qualified Data.Map as Map



type EColor         = Ess @TypeLevel "color"
type EColorWhite    = Ess @TypeLevel "color:white"
type EColorRed      = Ess @TypeLevel "color:red"
type EColorGreen    = Ess @TypeLevel "color:green"
type EColorBlue     = Ess @TypeLevel "color:blue"
type EColorRef      = Ess @TypeLevel "ref:color"

type EAvailableColors = Ess @TypeLevel "available colors"
type EAbstractLamp    = Ess @TypeLevel "lamp:abstract"
type EIsOn            = Ess @TypeLevel "is on"

type EDaylightLamp    = Ess @TypeLevel "lamp:daylight"
type ETableLamp       = Ess @TypeLevel "lamp:table"

type Color = StaticProp (Group EColor)

type ColorWhite = StaticProp (GroupRoot EColorWhite Color)
type ColorRed   = StaticProp (GroupRoot EColorRed Color)
type ColorGreen = StaticProp (GroupRoot EColorGreen Color)
type ColorBlue  = StaticProp (GroupRoot EColorBlue Color)

type ColorPath = '[ EAvailableColors, EColorWhite ]

type AbstractLamp = AbstractProp (Group EAbstractLamp)
  '[ PropKeyVal EIsOn (OwnVal (BoolValue False))

   , PropKeyBag EAvailableColors
      '[ OwnProp ColorWhite                   -- let's test
                                              -- two ways of specifying static props
       , OwnProp (StaticPropRef ColorRed)
       , OwnProp (StaticPropRef ColorGreen)
       , OwnProp (StaticPropRef ColorBlue)
       ]

   -- | Current color. Points to a possible color.
   , PropKeyVal EColor (OwnVal (PathValue ColorPath))
   ]

type DaylightLamp = DerivedProp EDaylightLamp AbstractLamp
  '[
  ]

type TableLamp = DerivedProp ETableLamp AbstractLamp
  '[
  ]



spec :: Spec
spec =
  describe "Abstract property deriving test" $ do
    it "Test" $ do
      sEnv <- makeSEnv DebugDisabled

      lamp <- sMat' sEnv () $ Proxy @DaylightLamp

      statProps <- readTVarIO $ seStaticPropertiesVar sEnv
      statEsss  <- readTVarIO $ seStaticEssencesVar sEnv

      print $ "Stat props: " <> show (Map.keys statProps)
      print $ "Stat essences: " <> show (Map.keys statEsss)

      case lamp of
        PropDict group props -> do
          let (ess, sId) = getComboPropertyId group
          length props `shouldBe` 3                   -- these numbers may be incorrect.
          length statProps `shouldBe` 7               -- needs double-checking.
          ess `shouldBe` Ess "lamp:daylight"
          Map.member ess statEsss `shouldBe` True
        _ -> error "invalid materialization result"
