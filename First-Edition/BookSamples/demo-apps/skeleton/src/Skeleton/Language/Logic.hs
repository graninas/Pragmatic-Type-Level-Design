-- | Domain types that describe business logic transformations.
module Skeleton.Language.Logic where

import Skeleton.Language.Domain
import Skeleton.Lib.TypeSelector

import GHC.TypeLits


data StaticTransform
  (thing :: (StaticThing (lvl :: Level)))
  = Transform (StringType lvl)

