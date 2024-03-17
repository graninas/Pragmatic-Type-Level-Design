{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Cellular.App.Storage.Rules where

import Cellular.Automaton
import Cellular.Language.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


-- | Existential data type for storing rule implementations.
data RuleImpl where
  RI :: IAutomaton rule => Proxy rule -> RuleImpl

supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri -> (getCode ri, ri))
  [ RI (Proxy :: Proxy SeedsRule)
  , RI (Proxy :: Proxy ReplicatorRule)
  , RI (Proxy :: Proxy GoLRule)
  ]

getCode :: RuleImpl -> RuleCode
getCode (RI proxy) = getCode' proxy

getCode' :: IAutomaton rule => Proxy rule -> RuleCode
getCode' proxy = code proxy

supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules

