{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE GADTs                    #-}

module Tests.MinesweeperSpec where

import CPrelude

import qualified Prelude

import Test.Hspec
import qualified Data.Map as Map
import GHC.TypeLits


type Minefield =
  '[ "7 6     A   @"
   , "  B 7 B     8"
   , "  C   7     B"
   , "  8     A 6  "
   , "B A   7 8   8"
   , "7     C   7 6"
   , "  A C     7 C"
   ]

spec :: Spec
spec = do
  describe "Minesweeper test" $ do

    xit "Dummy" $ do
      1 `shouldBe` 2


