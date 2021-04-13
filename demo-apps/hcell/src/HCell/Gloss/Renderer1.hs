module HCell.Gloss.Renderer1 where

import HCell.Prelude

import HCell.Types
import HCell.Gloss.Types
import HCell.Gloss.Conv
import HCell.Game.State
import HCell.Game.Debug

import qualified Data.Set as Set

import Graphics.Gloss

data RenderOptions = RenderOptions
  { debugOptions   :: DebugOptions
  , gridCellSize   :: GridCellSize
  , bareCellSize   :: BareCellSize
  , glossBaseShift :: GlossBaseShift
  , glossBareCellSize :: GlossBareCellSize
  }


deadCell :: Picture
deadCell = blank

aliveCell :: Picture
aliveCell = Color (greyN 0.8) $ circleSolid 2

cellBox :: GlossBareCellSize -> Color -> Picture
cellBox (GlossBareCellSize s) col = Color col $ rectangleWire s s

unknown :: GlossBareCellSize -> String -> Picture
unknown (GlossBareCellSize s) w = Color red $ Pictures
  [ rectangleWire s s
  , line [(nsDiv2, nsDiv2), (sDiv2, sDiv2)]
  , line [(nsDiv2, sDiv2), (sDiv2, nsDiv2)]
  ]
  where
    sDiv2 = s / 2
    nsDiv2 = negate sDiv2



renderLevel :: RenderOptions -> Level -> Picture
renderLevel (RenderOptions {..}) level =
  Pictures $ map (toGlossCell'' . withGlossCoords) $ Set.toList level
  where
    withGlossCoords :: Coords -> GlossCoords
    withGlossCoords cellPos = coordsToGlossCell glossBaseShift gridCellSize cellPos

    toGlossCell'' :: GlossCoords -> Picture
    toGlossCell'' (GlossCoords (shiftX, shiftY)) =
      if dbgShowCellBoxes debugOptions
      then Pictures [ Translate shiftX shiftY aliveCell
                    , Translate shiftX shiftY $ cellBox glossBareCellSize $ dbgCellBoxColor debugOptions
                    ]
      else Translate shiftX shiftY aliveCell


glossRenderer :: GameState -> IO Picture
glossRenderer (GameState {..}) = do
  (wndSize, bareCellSize, cellSpaceSize, level, dbgOpts) <- atomically $ do
    wndSize       <- readTVar wndSizeVar
    bareCellSize  <- readTVar bareCellSizeVar
    cellSpaceSize <- readTVar cellSpaceSizeVar
    level         <- readTVar levelVar
    dbgOpts       <- readTVar debugOptionsVar
    pure (wndSize, bareCellSize, cellSpaceSize, level, dbgOpts)

  let gridCellSize      = getGridCellSize bareCellSize cellSpaceSize
  let glossBaseShift    = getGlossBaseShift wndSize
  let bareCellHalf      = getBareCellHalf bareCellSize
  let glossGridCellSize = getGlossGridCellSize gridCellSize
  let glossBareCellSize = getGlossBareCellSize bareCellSize

  let renderOptions = RenderOptions
        dbgOpts
        gridCellSize
        bareCellSize
        glossBaseShift
        glossBareCellSize

  pure $ Pictures
    [ renderLevel renderOptions level
    ]
