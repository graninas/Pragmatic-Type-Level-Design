{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of the Free monadc interface.
module Turing.Machine.Implementation.FreeMonad.Dynamic where

import Turing.Machine.Language
import Turing.Machine.Interface.FreeMonad
import Turing.Machine.Implementation.Common
import Turing.Machine.Implementation.Dynamic

import Control.Monad.Free


-- | Pure interpreter of the Free monad interface for dynamic rule.

dynamicInterpret :: CustomRuleVL -> MachineMethod a -> a
dynamicInterpret rule (Run tape next) = next (runDynamicRule rule tape)
dynamicInterpret (Rule n _ _) (Name next) = next n

dynamicRuleInterpreter :: CustomRuleVL -> Machine a -> a
dynamicRuleInterpreter rule (Free m) = let
  next = dynamicInterpret rule m
  in dynamicRuleInterpreter rule next
dynamicRuleInterpreter _ (Pure a) = a

