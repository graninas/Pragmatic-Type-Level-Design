{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module ParsingGen5Spec where

import Cell
import Board
import Automaton
import Automata.GameOfLife

import Control.Monad
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.Constraint.Symbol
import GHC.TypeLits
import Test.Hspec

-- Can't have:
-- instance Automaton "GameOfLife" where
-- instance Automaton "B123 S24" where
-- instance Automaton "B3 S134" where
-- instance Automaton "B1357 S1357" where

-- Forced to have:
-- instance Automaton Hardcoded "GameOfLife" where
-- instance Automaton Hardcoded "Seeds" where
-- instance Automaton Hardcoded "Replicator" where
-- instance Automaton Generic rule where

data AutomatonParam
  = Hardcoded
  | Generic


-- N.B. Two params, all of them should appear in the methods.
class KnownSymbol rule => Automaton'
    (param :: AutomatonParam)
    (rule :: Symbol) where
  step' :: Proxy (param :: AutomatonParam) -> Proxy rule -> Board -> Board
  code' :: Proxy (param :: AutomatonParam) -> Proxy rule -> RuleCode
  name' :: Proxy (param :: AutomatonParam) -> Proxy rule -> String
  name' _ proxy = symbolVal proxy

instance Automaton' 'Hardcoded GoLRule where
  step' _ _ = error "Not implemented"
  code' _ _ = error "Not implemented"
  name' _ _ = error "Not implemented"

spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      print "Hey!"

