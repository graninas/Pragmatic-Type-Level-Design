module Skeleton.App.Package.Thing where

import qualified Skeleton.Language as L

import qualified Data.Aeson as A

-- | External transformation & data package definitions

data Thing = Thing
  { thingName :: String
  }
  deriving (Read, Show, Eq, Ord, A.ToJSON, A.FromJSON)

data TransformedThing = TransformedThing
  { transformedThingName :: String
  }
  deriving (Read, Show, Eq, Ord, A.ToJSON, A.FromJSON)


toDynamicThing :: Thing -> L.CustomThing
toDynamicThing (Thing ) = let
    qs' = map fromIntegral qs
    cond' = L.NeighborsCount (fromIntegral st) qs'
  in L.StateTransition (fromIntegral from) (fromIntegral to) cond'

toDynamicRule :: Rule -> L.DynamicRule
toDynamicRule (Rule n c (AdjacentsLvl lvl) ds ts) = let
  nh' = L.AdjacentsLvl (fromIntegral lvl)
  ds' = L.DefState (fromIntegral ds)
  ts' = map toCustomStateTransition ts

  dynStep = L.DynamicStep ds' ts'
  in L.DynamicRule n c nh' dynStep

