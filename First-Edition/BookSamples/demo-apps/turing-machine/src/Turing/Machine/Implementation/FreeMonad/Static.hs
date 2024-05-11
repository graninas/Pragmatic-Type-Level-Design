{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of the Free monadc interface.
module Turing.Machine.Implementation.FreeMonad.Static where

import Turing.Machine.Language
import Turing.Machine.Interface.FreeMonad
import Turing.Machine.Implementation.Common
import Turing.Machine.Implementation.Static

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )

import Control.Monad.Free


-- | Pure interpreter of the Free monad interface for static rule.

staticInterpret
  :: RuleRunner rule
  => Proxy (rule :: CustomRuleTL) -> MachineMethod a -> a
staticInterpret ruleProxy (Run tape next) = next (runRule ruleProxy tape)
staticInterpret ruleProxy (Name next) = next (runRuleName ruleProxy)

staticRuleInterpreter
  :: RuleRunner rule
  => Proxy (rule :: CustomRuleTL) -> Machine a -> a
staticRuleInterpreter ruleProxy (Free m) = let
  next = staticInterpret ruleProxy m
  in staticRuleInterpreter ruleProxy next
staticRuleInterpreter _ (Pure a) = a

