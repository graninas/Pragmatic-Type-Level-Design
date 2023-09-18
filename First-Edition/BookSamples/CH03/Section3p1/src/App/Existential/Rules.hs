{-# LANGUAGE GADTs #-}
module App.Existential.Rules where

import Domain.Automaton ( Automaton(code), RuleCode )
import Assets.Automata.GameOfLife ( GoLRule )
import Assets.Automata.Seeds ( SeedsRule )
import Assets.Automata.Replicator ( ReplicatorRule )

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

supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules

-- Won't work:
-- extractProxy :: Automaton rule => RuleImpl -> Proxy rule
-- extractProxy (RI proxy) = proxy
