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

data ObjectInfo = ObjectInfo
  { oiObjectType :: ObjectType
    -- ^ Object template id

  , oiObjectId :: Maybe ObjectId
    -- ^ Object identifier among runtime objects

  , oiEnabled :: Bool
    -- ^ If the object is disabled, it doesn't participate in events
    -- and doesn't show its icon

  , oiPos :: Maybe Pos
    -- ^ Object position (if available)

  , oiIcons :: (Icon, [OverhaulIcon])
  -- ^ Base icon and all active icons, head icon is top.
  --   Should only display icons from this list
  }
  deriving (Show, Eq, Ord)

-- Specific verbs for object templates
-- TODO: find a better place
data GetIcon       = GetIcon
data GetObjectInfo = GetObjectInfo
data GetObjectType = GetObjectType
