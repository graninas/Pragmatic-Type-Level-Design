module Skeleton.Machine.Static where

import Skeleton.Machine.Static

import Skeleton.Machine.Language

import GHC.TypeLits



class Runner (rule :: CustomRule) where
  run
    :: Proxy rule
    -> Int
    -> Tape
    -> Tape


instance Runner ('Rule states) where
  run _ n tape =
    if n <= 0
    then tape
    else


instance MakeTransform 'Finish where
  makeTransform _ s = s

instance MakeTransform ('Take (n :: Nat) next) where
  makeTransform _ s = let
    n = fromIntegral (natVal (Proxy @n))
    s' = take n s
    in makeTransform (Proxy @next) s'

instance MakeTransform ('Drop (n :: Nat) next) where
  makeTransform _ s = let
    n = fromIntegral (natVal (Proxy @n))
    s' = take n s
    in makeTransform (Proxy @next) s'
