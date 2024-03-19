{-# LANGUAGE DataKinds #-}

module Turing.Machine.Interface
  ( IMachine
  , ITape
  , run
  , initTape
  ) where

import Turing.Machine.Language
import Turing.Machine.Runner.Static

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


class ITape s where
  initTape :: s -> Tape

instance ITape String where
  initTape s = Tape s           -- TODO


instance
  ( RuleRunner rule
  ) =>
  IMachine () rule where
  run () = runRule


