module Minefield.Core.Object where

import CPrelude

import Minefield.Core.Types

import GHC.TypeLits


-- | Stack of extra icons.
-- Icons are being displayed starting from the top.
-- Self-destructs after N ticks.

data OverhaulIcon = OverhaulIcon
  { ovhParentObjectId :: Maybe ObjectId
  , ovhIcon :: Icon
  , ovhTicks :: Maybe Int
  }
  deriving (Show, Eq, Ord)

-- | Icon batch. Ticks simultaneously with other batches.
-- Contains icons that tick sequentially.
newtype OverhaulIconBatch = OverhaulIconBatch [OverhaulIcon]

-- | Object info.
data ObjectInfo = ObjectInfo
  { oiObjectType :: ObjectType
    -- ^ Object template id

  , oiObjectId :: ObjectId
    -- ^ Object identifier among runtime objects

  , oiEnabled :: Bool
    -- ^ If the object is disabled, it doesn't participate in events
    -- and doesn't show its icon

  , oiIcons :: (Icon, [IORef OverhaulIconBatch])
  -- ^ (Base icon, batches).
  --    Batches tick simultaneously.
  --    Batch icons tick sequentially.
  }
  deriving (Show, Eq, Ord)

-- Specific verbs for object templates
-- TODO: find a better place
data GetIcon       = GetIcon
data GetObjectInfo = GetObjectInfo
data GetObjectType = GetObjectType
