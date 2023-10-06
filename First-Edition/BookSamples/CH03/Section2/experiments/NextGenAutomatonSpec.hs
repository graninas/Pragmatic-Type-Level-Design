{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
module NextGenAutomatonSpec where

import Domain.Cell
import Domain.Board
import Domain.Automaton
import Assets.Automata.GameOfLife

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
-- instance Automaton 'Hardcoded "GameOfLife" where
-- instance Automaton 'Hardcoded "Seeds" where
-- instance Automaton 'Hardcoded "Replicator" where
-- instance Automaton 'Custom rule where

data AutomatonParam
  = Hardcoded
  | Custom

-- N.B. Two params, all of them should appear in the methods.
class KnownSymbol rule => Automaton'
    (param :: AutomatonParam)
    (rule :: Symbol) where
  step' :: Proxy (param :: AutomatonParam) -> Proxy rule -> Board -> Board

instance Automaton' 'Hardcoded GoLRule where
  step' _ _ _ = error "Not implemented"

instance Automaton' 'Custom "B135 S246" where
  step' _ _ _ = error "Not implemented"


data AutomatonParam'' param
  = Hardcoded'' param
  | Custom'' param

class Automaton'' (param :: AutomatonParam'' p) where

instance Automaton'' ('Hardcoded'' GoLRule) where
instance Automaton'' ('Custom'' "B135 S246") where
instance Automaton'' ('Custom'' Bool) where    -- Regular types work, too, not only Symbols




data HardcodedRuleNG (param :: Symbol)
data CustomRuleNG param
data LifeLike bRule sRule

class AutomatonNG param where

instance AutomatonNG (HardcodedRuleNG GoLRule) where
instance AutomatonNG (CustomRuleNG (LifeLike [] [])) where
instance AutomatonNG (CustomRuleNG Bool) where    -- Regular types work, too, not only Symbols


data RuleTypeNNG where
  HardcodedRuleNNG :: param -> RuleTypeNNG
  LifeLikeRuleNNG :: rule -> RuleTypeNNG
  GenericRuleNNG
    -- :: (states :: States)     -- Can't place kinds here for some reason
    :: states
    -> grid
    -> rule
    -> RuleTypeNNG
  CustomRuleNNG :: param -> RuleTypeNNG

class AutomatonNNG (param :: RuleTypeNNG) where

-- instance AutomatonNNG Int where      -- kinds don't match

instance AutomatonNNG (HardcodedRuleNNG GoLRule) where
instance AutomatonNNG (CustomRuleNNG "B135 S246") where
instance AutomatonNNG (CustomRuleNNG Bool) where    -- Regular types work, too, not only Symbols

instance AutomatonNNG (GenericRuleNNG Int Char String) where
instance AutomatonNNG (GenericRuleNNG [Nat] [Nat] [Nat]) where



-- Rule parsing efficiency problem
--
-- CustomRule "B135 S246"
--
-- The Symbol "B135 S246" can be transformed into a regular string
-- and then parsed. If it's in the step function, then it happens
-- all the time => inefficient.
--
-- The Symbol "B135 S246" can be parsed on the type level.
-- This will happen once, and will not require re-parsing on every run.





spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      print "Hey!"

