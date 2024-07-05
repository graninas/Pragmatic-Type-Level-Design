{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE TypeFamilies             #-}

module TypeLevel.InterfacesSpec where

import CPrelude

import TypeLevel.Interfaces

import GHC.TypeLits

import Test.Hspec


type EAbstractDoor = Essence "object:abstract door"
type ESpecificDoor = Essence "object:specific door"
type EIcon         = Essence "system:icon"
type EPos          = Essence "intrinsics:pos"
type EHP           = Essence "intrinsics:hp"

type IconVal (s :: Symbol) = Dummy
type HPTagVal (n :: Nat) = Dummy
type PosTagVal (x :: Nat) (y :: Nat) = Dummy

type AbstractDoor = AbstractProp (Group EAbstractDoor)
  '[ KeyValField EIcon (OwnVal (IconVal "+"))
   , KeyValField EHP   (OwnVal (HPTagVal 50))
   , KeyValField EPos  (OwnVal (PosTagVal 0 0))
   ]

type SpecificDoor = DerivedProp ESpecificDoor AbstractDoor
  '[ KeyValField EIcon (OwnVal (IconVal "?"))
   , KeyValField EHP   (OwnVal (HPTagVal 50))
   , KeyValField EPos  (OwnVal (PosTagVal 2 3))
   ]

spec :: Spec
spec = do
  describe "Type level interfaces" $ do
    xit "Property materialization test" $ do
      1 `shouldBe` 2
