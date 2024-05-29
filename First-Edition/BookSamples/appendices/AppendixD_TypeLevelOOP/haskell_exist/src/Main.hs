{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model

import GHC.TypeLits
import TypeSelector.Granular

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



main :: IO ()
main = pure ()
