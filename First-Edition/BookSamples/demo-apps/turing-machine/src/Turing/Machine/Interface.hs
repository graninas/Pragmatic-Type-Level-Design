{-# LANGUAGE DataKinds #-}

module Turing.Machine.Interface
  ( IMachine
  , run
  , name
  ) where

import Turing.Machine.Language

import GHC.TypeLits
import Data.Proxy (Proxy(..))


-- | Interface for Turing Machines.
-- This interface is needed to unify static implementations.

class IMachine
  payload
  (rule :: CustomRule) where
  run
    :: payload
    -> Proxy rule
    -> Tape
    -> Either String Tape
  name
    :: payload
    -> Proxy rule
    -> String

