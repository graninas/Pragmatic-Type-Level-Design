{-# LANGUAGE DataKinds #-}

module Turing.Machine.Interface
  ( IMachine
  , run
  ) where

import Turing.Machine.Language

-- TODO: this is a strange dependency.
-- Interface should not depend on the implementation.
import Turing.Machine.Implementation.Static (RuleRunner, runRule)

import GHC.TypeLits
import Data.Proxy (Proxy(..))


class IMachine
  payload
  (rule :: CustomRule) where
  run
    :: payload
    -> Proxy rule
    -> Tape
    -> Tape



-- TODO: this is a strange dependency.
-- Interface should not depend on the implementation.
instance
  ( RuleRunner rule
  ) =>
  IMachine () rule where
  run () = runRule
