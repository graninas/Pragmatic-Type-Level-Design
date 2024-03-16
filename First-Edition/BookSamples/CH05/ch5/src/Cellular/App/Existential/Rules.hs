{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cellular.App.Existential.Rules where

import Cellular.Automaton
import Cellular.Language.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl where
  RI
    :: IAutomaton () rule
    => Proxy rule
    -> RuleImpl
  DynRI
    :: IAutomaton DynamicRule 'DynRule
     => DynamicRule
     -> RuleImpl

supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri -> (getCode ri, ri))
  [ RI (Proxy :: Proxy SeedsRule)
  , RI (Proxy :: Proxy ReplicatorRule)
  , RI (Proxy :: Proxy GoLRule)
  ]

getCode :: RuleImpl -> RuleCode
getCode (RI proxy) = getCode' proxy
getCode (DynRI dynRule) = code dynRule (Proxy @'DynRule)

getCode' :: IAutomaton () rule => Proxy rule -> RuleCode
getCode' proxy = code () proxy

supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules

