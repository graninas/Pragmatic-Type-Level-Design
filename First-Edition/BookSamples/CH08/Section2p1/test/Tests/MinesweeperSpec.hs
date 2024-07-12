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



type TestMinefield =
  '[ "6 6     B    "
   , "  B 7 B     8"
   , "  B   7     B"
   , "  8     B 6  "
   , "B B   7 8   6"
   , "7     B   7 6"
   , "  B B     6 B"
   ]



spec :: Spec
spec = do
  describe "Minesweeper test" $ do

    xit "Dummy" $ do
      1 `shouldBe` 2


