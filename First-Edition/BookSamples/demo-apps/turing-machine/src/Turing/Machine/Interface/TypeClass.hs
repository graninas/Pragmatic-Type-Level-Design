{-# LANGUAGE DataKinds #-}

{- This module demonstrates type class as an interface-like
abstraction. -}

module Turing.Machine.Interface.TypeClass
  ( IMachine
  , run
  , name
  ) where

import Turing.Machine.Language

import GHC.TypeLits
import Data.Proxy (Proxy(..))


-- | Interface for Turing Machines based on a type class.

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

