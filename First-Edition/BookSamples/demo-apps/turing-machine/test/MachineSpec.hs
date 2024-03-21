{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module MachineSpec where

import Prelude hiding ((<>))

import Turing.Machine.Language
import Turing.Machine.Interface
import Turing.Machine.Implementation.Static
import Turing.Assets.SimpleRule
import Turing.Assets.BinaryIncrement

import Test.Hspec
import Data.Proxy
import GHC.TypeLits
import qualified Data.Map as Map


spec :: Spec
spec = do
  describe "Machine tests" $ do
    it "Simple rule 1" $ do
      let tape1  = initTape ("BA" :: String)
      let tape2' = initTape [Left Blank, Right 'B', Right 'A']
      let eTape2 = run () (Proxy @SimpleRule) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'
    it "Simple rule 2" $ do
      let tape1  = initTape ("AB" :: String)
      let tape2' = initTape [Left Blank, Right 'B', Right 'B']
      let eTape2 = run () (Proxy @SimpleRule) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'
    it "Simple rule 3" $ do
      let tape1  = shiftHeadRight $ initTape ("BA" :: String)
      let tape2' = initTape ("CB" :: String)
      let eTape2 = run () (Proxy @SimpleRule) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'
    it "Simple rule 4" $ do
      let tape1  = shiftHeadRight $ initTape ("BB" :: String)
      let tape2' = initTape ("CB" :: String)
      let eTape2 = run () (Proxy @SimpleRule) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'
    it "Simple rule 5" $ do
      let tape1  = shiftHeadRight $ initTape ("CB" :: String)
      let tape2' = initTape ("CB" :: String)
      let eTape2 = run () (Proxy @SimpleRule) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'

    it "Binary Increment" $ do
      let tape1  = initTape ("100101" :: String)
      let tape2' = initTape ("100110" :: String)
      let eTape2 = run () (Proxy @BinaryIncrement) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'

    it "Binary Increment invalid input" $ do
      let tape1  = initTape ("ABCDEF" :: String)
      let eTape2 = run () (Proxy @BinaryIncrement) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Left "[1] Rule should start from a digit."
