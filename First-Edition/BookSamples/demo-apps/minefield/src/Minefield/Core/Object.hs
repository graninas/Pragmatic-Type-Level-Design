module Minefield.Core.Object where

import CPrelude

import Minefield.Core.Types

import GHC.TypeLits


-- | Stack of extra icons.
-- The top one should be displayed instead of the main one.
-- Self-destructs after N turns and M ticks.
-- N == -1 means "forever"

data OverhaulIcon = OverhaulIcon
  { ovhIcon  :: Icon
  , ovhTurns :: TurnsCount
  , ovhTicks :: TicksCount
  }
  deriving (Show, Eq, Ord)


data ObjectInfo = ObjectInfo
  { oiIcon :: Icon
  , oiPos  :: Pos
  , oiObjectType :: ObjectType
  , oiEnabled :: Bool
  , oiOverhaulIcons :: [OverhaulIcon]
  }
  deriving (Show, Eq, Ord)
