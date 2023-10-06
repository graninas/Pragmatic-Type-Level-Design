{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module ParsingGen5Spec where

import Board

import Control.Monad
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.Constraint.Symbol
import GHC.TypeLits
import Test.Hspec


mergePSymbols
  :: forall s1 s2
   . (KnownSymbol s1, KnownSymbol s2)
  => Proxy (s1 :: Symbol)
  -> Proxy (s2 :: Symbol)
  -> Proxy (AppendSymbol s1 s2)
mergePSymbols _ _ = Proxy :: Proxy (AppendSymbol s1 s2)

printPSymbol :: KnownSymbol s => Proxy (s :: Symbol) -> String
printPSymbol = symbolVal

x :: Proxy "AB"
x = mergePSymbols (Proxy @"A") (Proxy @"B")







data CustomRule (m :: Symbol)

type family Parse (m :: Symbol) :: CustomRule m       -- PolyKinds


class EvalParse m where

instance EvalParse (CustomRule m) where

-- step' :: Proxy (s :: CustomRule) -> Board -> Board
-- step' _ board = board

spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      -- let board = step' (Proxy @(Parse "B12")) Map.empty

      -- board `shouldBe` Map.empty

      print "Hi!"

