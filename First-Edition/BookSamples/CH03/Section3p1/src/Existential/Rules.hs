{-# LANGUAGE GADTs #-}
module Existential.Rules where

import Automaton ( Automaton(code), RuleCode )
import Automata.GameOfLife ( GoLRule )
import Automata.Seeds ( SeedsRule )
import Automata.Replicator ( ReplicatorRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl where
  RI :: Automaton rule => Proxy rule -> RuleImpl

supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri -> (getCode ri, ri))
  [ RI (Proxy :: Proxy SeedsRule)
  , RI (Proxy :: Proxy ReplicatorRule)
  , RI (Proxy :: Proxy GoLRule)
  ]

getCode :: RuleImpl -> RuleCode
getCode (RI proxy) = getCode' proxy

getCode' :: Automaton rule => Proxy rule -> RuleCode
getCode' proxy = code proxy

type Rules = Map.Map RuleCode RuleImpl

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules
