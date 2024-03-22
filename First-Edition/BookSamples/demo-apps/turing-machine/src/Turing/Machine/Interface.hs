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

class IMachine
  payload
  (rule :: CustomRule lvl) where
  run
    :: payload
    -> Proxy rule
    -> Tape
    -> Either String Tape
  name
    :: payload
    -> Proxy rule
    -> String

