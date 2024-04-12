{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dynamic implementation of the IMachine type class interface.
-- Addresses dynamic value-level rules.

module Turing.Machine.Implementation.TypeClass.Dynamic where

import Turing.Machine.Language
import Turing.Machine.Interface.TypeClass
import Turing.Machine.Implementation.Common
import Turing.Machine.Implementation.Dynamic

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


instance
  IMachine CustomRuleVL DynamicRule where
  run rule _ = runDynamicRule rule
  name (Rule n _ _) _ = n
  name DynamicRuleTag _ = error "DynamicRuleTag placeholder doesn't have name."
