module Skeleton.Interface where

import Skeleton.Language.Domain
import Skeleton.Language.Data

import GHC.TypeLits


class IInterface
  dynPayload
  (staticThing :: Thing) where
  transform :: dynPayload -> Data staticThing -> Data staticThing




