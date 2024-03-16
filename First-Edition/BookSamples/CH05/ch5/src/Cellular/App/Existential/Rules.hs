{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cellular.App.Existential.Rules where

import Cellular.Automaton
import Cellular.Language.Automaton

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

type Rules = Map.Map RuleCode RuleImpl


getCode :: RuleImpl -> RuleCode
getCode (RI proxy) = getCode' proxy
getCode (DynRI dynRule) = code dynRule (Proxy @'DynRule)

getCode' :: IAutomaton () rule => Proxy rule -> RuleCode
getCode' proxy = code () proxy

