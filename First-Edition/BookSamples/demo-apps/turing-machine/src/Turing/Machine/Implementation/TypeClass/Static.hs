{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Static implementation of the IMachine type class interface.
-- Addresses static type-level hardcoded rules.

module Turing.Machine.Implementation.TypeClass.Static where

import Turing.Machine.Language
import Turing.Machine.Interface.TypeClass
import Turing.Machine.Implementation.Common
import Turing.Machine.Implementation.Static

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


-- N.B. It has to be orphan...

instance
  ( RuleRunner rule
  , rule ~ 'Rule name idx ss
  , KnownSymbol name
  ) =>
  IMachine () rule where
  run () = runRule
  name () = runRuleName
