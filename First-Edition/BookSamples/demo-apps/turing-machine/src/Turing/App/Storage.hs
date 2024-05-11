{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Types for rules and tapes storage.

module Turing.App.Storage where

import Turing.Machine.Interface.TypeClass
import Turing.Machine.Interface.FreeMonad
import Turing.Machine.Language
import Turing.Machine.Implementation.TypeClass.Static
import Turing.Machine.Implementation.TypeClass.Dynamic
import Turing.Machine.Implementation.FreeMonad.Static
import Turing.Machine.Implementation.FreeMonad.Dynamic

import Lib.TypeSelector
import qualified Data.Map as Map
import Data.Proxy

type TapeIndex = Int
type Tapes = Map.Map TapeIndex Tape


-- | Runtime rule instance.
data RuleImpl where
  -- | Rule instance for statically defined hardcoded type-level rules.
  -- Utilizes type class as an interface.
  TypeClassRI
    :: (IMachine () (rule :: CustomRuleTL)

        -- N.B., Materialization requirement here is to showcase the pattern.
       , Materialize () rule CustomRuleVL
    ) => Proxy rule
      -> RuleImpl
  -- | Rule instance for dynamic value-level rules.
  -- Utilizes type class as an interface.
  TypeClassDynRI
    :: IMachine CustomRuleVL DynamicRule
    => CustomRuleVL
    -> RuleImpl

  -- | Rule instance for dynamic value-level rules.
  -- Utilizes Free monad as an interface and uses its only implementation.
  FreeMonadRI
    :: (forall a. Machine a -> a)
    -> RuleImpl

type RuleIndex = Int

type Rules = Map.Map RuleIndex (RuleImpl, String)

getName :: RuleImpl -> String
getName (TypeClassRI proxy) = name () proxy
getName (TypeClassDynRI rule) = name rule $ Proxy @DynamicRule
getName (FreeMonadRI ruleInterpreter) = ruleInterpreter nameFM
