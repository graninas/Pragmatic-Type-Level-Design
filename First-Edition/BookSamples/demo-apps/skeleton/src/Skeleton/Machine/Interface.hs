module Skeleton.Machine.Interface
  ( IMachine
  , run
  ) where

import Skeleton.Machine.Language

import GHC.TypeLits


class IMachine
  payload
  (rule :: CustomRule) where
  run :: payload -> Proxy rule -> Int -> Tape -> Tape


instance IMachine () rule where
  run () = runStaticRule


runStaticRule
  :: forall rule
   . Runner rule
  => Proxy rule
  -> Int
  -> Tape
  -> Tape
runStaticRule proxy maxSteps tape = tape     --- TODO

