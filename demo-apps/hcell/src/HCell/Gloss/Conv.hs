module HCell.Gloss.Conv where

import CPrelude

import HCell.Types
import HCell.Gloss.Types


coordsToGlossCell :: GlossBaseShift -> GridCellSize -> Coords -> GlossCoords
coordsToGlossCell
  (GlossBaseShift (shiftX, shiftY))
  (GridCellSize cellSize)
  (x, y)
  = GlossCoords (x', y')
  where
    x' = shiftX + fromIntegral (x * cellSize)
    y' = shiftY + fromIntegral (y * cellSize)

getBaseShift :: GlossWindowSize -> BaseShift
getBaseShift (GlossWindowSize (wX, wY)) = BaseShift (shiftX, shiftY)
  where
    shiftX = negate $ wX `div` 2
    shiftY = negate $ wY `div` 2

getGlossBaseShift :: GlossWindowSize -> GlossBaseShift
getGlossBaseShift gwnd = GlossBaseShift (fromIntegral shiftX, fromIntegral shiftY)
  where
    BaseShift (shiftX, shiftY) = getBaseShift gwnd

getBareCellHalf :: BareCellSize -> BareCellHalf
getBareCellHalf (BareCellSize s) = BareCellHalf $ s `div` 2

getGridCellSize :: BareCellSize -> CellSpaceSize -> GridCellSize
getGridCellSize (BareCellSize bareCellSize) (CellSpaceSize cellSpaceSize) =
  GridCellSize $ bareCellSize + cellSpaceSize

getGlossGridCellSize :: GridCellSize -> GlossGridCellSize
getGlossGridCellSize (GridCellSize s) = GlossGridCellSize $ fromIntegral s

getGlossBareCellSize :: BareCellSize -> GlossBareCellSize
getGlossBareCellSize (BareCellSize s) = GlossBareCellSize $ fromIntegral s
