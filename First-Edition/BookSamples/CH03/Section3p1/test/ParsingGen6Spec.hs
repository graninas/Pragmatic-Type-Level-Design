{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE PolyKinds #-}
module ParsingGen6Spec where

import Board

import Control.Monad
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.Constraint.Symbol
import GHC.TypeLits
import Test.Hspec

-- Representing rule digits as type-level lists of Nat
type RuleDigits = [Nat]

-- New type to hold custom and built-in rules
data RuleType
  = Builtin Symbol
  | Custom RuleDigits RuleDigits

-- A type-level eDSL to convert a Symbol to RuleType
type family ParseRule (rule :: Symbol) :: RuleType where
  -- your type-level parsing logic here
  -- Example: ParseRule "B135/S135" = Custom '[1,3,5] '[1,3,5]

-- Redesign of CellWorld to accept RuleType
newtype CellWorld (rule :: RuleType) = CW Board

-- The Automaton type class takes in RuleType
class Automaton (rule :: RuleType) where
  -- step :: CellWorld rule -> CellWorld rule
  -- code :: Proxy rule -> RuleCode
  -- name :: Proxy rule -> String

-- Example instances for builtin rules
instance Automaton (Builtin "Game of Life") where
  -- ...

-- For custom rules
instance (KnownNat n, KnownNat m)
  => Automaton (Custom '[n] '[m]) where
  -- ...



-- type family ParseCustomRule (s :: Symbol) :: RuleType where
--   ParseCustomRule s = Custom
--     (ParseNumbers (TakeWhile (/= "/") s))
--     (ParseNumbers (DropWhile (/= "/") s))

-- -- TakeWhile and DropWhile are assumed type-level functions
-- -- that operate on type-level lists of symbols (or Nats)

-- type family ParseNumbers (s :: Symbol) :: [Nat] where
--   ParseNumbers "" = '[]
--   ParseNumbers s = ConsNat (ParseNumber (Head s)) (ParseNumbers (Tail s))

-- -- Head and Tail are assumed type-level functions that give you the
-- -- first symbol and the remaining symbols of a Symbol.


spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      -- let board = step' (Proxy @(Parse "B12")) Map.empty

      -- board `shouldBe` Map.empty

      print "Hi!"

