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
import Turing.Assets.BinaryIncrement

import Test.Hspec
import Data.Proxy
import GHC.TypeLits
import qualified Data.Map as Map


spec :: Spec
spec = do
  describe "Machine tests" $ do
    it "Binary Increment" $ do
      let tape1  = initTape ("100101" :: String)
      let tape2' = initTape ("100110" :: String)
      let eTape2 = run () (Proxy @BinaryIncrement) tape1
      (shrinkBlanks <$> eTape2) `shouldBe` Right tape2'
