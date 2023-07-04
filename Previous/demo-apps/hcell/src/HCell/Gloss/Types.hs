module HCell.Gloss.Types where

import CPrelude

import HCell.Types

-- X is horizontal, grows to right
-- Y is vertical, grows to up
-- (-x, -y) is the left bottom corner
-- (0, 0) is in the center of the window

newtype GlossCoords     = GlossCoords (Float, Float)

-- Moves (0, 0) to the left down corner
newtype GlossBaseShift  = GlossBaseShift (Float, Float)

newtype GlossWindowSize = GlossWindowSize Coords
newtype GlossWindowPosition = GlossWindowPosition Coords
newtype GlossBareCellSize = GlossBareCellSize Float
newtype GlossGridCellSize = GlossGridCellSize Float

newtype GlossTextScale = GlossTextScale Float
