{-# LANGUAGE DataKinds #-}

module Skeleton.Machine.Interface
  ( IMachine
  , run
  ) where

import Skeleton.Machine.Language
import Skeleton.Machine.Runner.Static

import GHC.TypeLits
import Data.Proxy (Proxy(..))


class IMachine
  payload
  (rule :: CustomRule) where
  run :: payload -> Proxy rule -> Int -> Tape -> Tape


instance
  ( RuleRunner rule
  ) =>
  IMachine () rule where
  run () = runStaticRule


runStaticRule
  :: RuleRunner rule
  => Proxy rule
  -> Int
  -> Tape
  -> Tape
runStaticRule proxy maxSteps tape = tape     --- TODO

