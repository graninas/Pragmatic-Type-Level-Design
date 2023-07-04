module HCell.Game.Debug where

import CPrelude

import HCell.Types
import HCell.Gloss.Types

import Graphics.Gloss

import qualified Data.Map as Map

data DebugOptions = DebugOptions
  { dbgShowCellBoxes   :: Bool
  , dbgCellBoxColor    :: Color
  , dbgShowGrid        :: Bool
  , dbgGridColor       :: Color
  }
