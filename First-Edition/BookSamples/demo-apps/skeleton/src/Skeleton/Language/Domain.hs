-- | Domain types that describe domain notions.
module Skeleton.Language.Domain where

import Skeleton.Lib.TypeSelector

import GHC.TypeLits


data ThingTemplate (lvl :: Level) = Thing
  { thingName :: StringType lvl
  }

data TransformedThingTemplate (lvl :: Level) = TransformedThing
  { transformedThingName :: StringType lvl
  }


type StaticThing = ThingTemplate 'TypeLevel
type DynamicThing = ThingTemplate 'ValueLevel

type StaticTransformedThing = TransformedThingTemplate 'TypeLevel
type DynamicTransformedThing = TransformedThingTemplate 'ValueLevel
