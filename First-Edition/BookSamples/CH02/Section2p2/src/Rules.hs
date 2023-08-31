{-# LANGUAGE GADTs #-}
module Rules where

import Automaton ( Automaton(code), RuleCode )
import GameOfLife ( GoLRule )
import Seeds ( SeedsRule )
import Replicator ( ReplicatorRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl where
  RuleImpl :: Automaton rule => Proxy rule -> RuleImpl

supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri@(RuleImpl proxy) -> (code proxy, ri))
  [ RuleImpl (Proxy :: Proxy SeedsRule)
  , RuleImpl (Proxy :: Proxy ReplicatorRule)
  , RuleImpl (Proxy :: Proxy GoLRule)
  ]

supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules
