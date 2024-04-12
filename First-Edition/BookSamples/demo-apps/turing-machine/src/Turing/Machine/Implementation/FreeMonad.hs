{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of the Free monadc interface.
module Turing.Machine.Implementation.FreeMonad where

import Turing.Machine.Language
import Turing.Machine.Interface.FreeMonad
import Turing.Machine.Implementation.Common
import Turing.Machine.Implementation.Dynamic

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )

import Control.Monad.Free


-- | Pure interpreter of the Free monad based rule.

interpret :: CustomRuleVL -> MachineMethod a -> a
interpret rule (Run tape next) = next (runDynamicRule rule tape)
interpret (Rule n _ _) (Name next) = next n

ruleInterpreter :: CustomRuleVL -> Machine a -> a
ruleInterpreter rule (Free m) = let
  next = interpret rule m
  in ruleInterpreter rule next
ruleInterpreter _ (Pure a) = a

