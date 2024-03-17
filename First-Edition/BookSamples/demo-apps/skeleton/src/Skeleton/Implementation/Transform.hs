module Skeleton.Implementation.Transform where

import Skeleton.Language

import GHC.TypeLits



class MakeTransform
  (StaticTransform (thing :: (StaticThing 'TypeLevel))) where
  makeTransform
    :: Proxy thing
    ->
