{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Types for rules and tapes storage.

module Turing.App.Storage where

import Turing.Machine.Interface
import Turing.Machine.Language
import Turing.Machine.Implementation.Static
import Turing.Machine.Implementation.Dynamic

import Lib.TypeSelector
import qualified Data.Map as Map
import Data.Proxy

type TapeIndex = Int
type Tapes = Map.Map TapeIndex Tape


data RuleImpl where
  RI
    :: ( IMachine () (rule :: CustomRuleTL)

        -- N.B., Materialization requirement here is to showcase the pattern.
       , Materialize () rule CustomRuleVL
    ) => Proxy rule
      -> RuleImpl
  DynRI
    :: IMachine (CustomRule 'ValueLevel) DynamicRule
    => CustomRule 'ValueLevel
    -> RuleImpl

type RuleIndex = Int
type Rules = Map.Map RuleIndex RuleImpl


getName :: RuleImpl -> String
getName (RI proxy) = name () proxy
getName (DynRI rule) = name rule $ Proxy @DynamicRule

isStatic :: RuleImpl -> Bool
isStatic (RI _) = True
isStatic _ = False
