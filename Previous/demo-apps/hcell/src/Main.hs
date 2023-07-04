module Main where

import CPrelude

import HCell.Types
import HCell.Gloss.Types
import HCell.Gloss.Renderer1
import HCell.Game.Logic
import HCell.Game.State
import HCell.Game.Debug

import qualified Data.Set as Set
import qualified Data.Text as T

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

defaultGlossWindowSize :: GlossWindowSize
defaultGlossWindowSize = GlossWindowSize (1200, 1200)

defaultGlossWindowPosition :: GlossWindowPosition
defaultGlossWindowPosition = GlossWindowPosition (500, 100)

-- | Number of cells in the grid
defaultGridDimensions :: GridDimensions
defaultGridDimensions = GridDimensions $ CellIdxs (50, 50)

defaultBareCellSize :: BareCellSize
defaultBareCellSize = BareCellSize 15

defaultCellSpaceSize :: CellSpaceSize
defaultCellSpaceSize = CellSpaceSize $ dcs `div` 10
  where
    (BareCellSize dcs) = defaultBareCellSize

defaultDbgOptions :: DebugOptions
defaultDbgOptions = DebugOptions True white True (dark green)

initGame
  :: GlossWindowSize
  -> GlossWindowPosition
  -> GridDimensions
  -> BareCellSize
  -> CellSpaceSize
  -> AliveCells
  -> DebugOptions
  -> IO (GameState, Display)
initGame
  (GlossWindowSize wndSize)
  (GlossWindowPosition wndPos)
  gridDims
  bareCellSize
  cellSpaceSize
  aliveCells
  dbgOpts = do
    let glossWindow = InWindow "Cellular automata" wndSize wndPos
    st <- GameState
      <$> newTVarIO (GlossWindowSize wndSize)
      <*> newTVarIO gridDims
      <*> newTVarIO bareCellSize
      <*> newTVarIO cellSpaceSize
      <*> newTVarIO aliveCells
      <*> newTVarIO dbgOpts
    pure (st, glossWindow)


loadBoard :: String -> IO AliveCells
loadBoard fName = do
  l1 :: [String] <- (reverse . map T.unpack . lines) <$> (readFile fName)
  let l2 :: [(Int, String)] = zip [1..] l1
  let l3 :: [ (Coords, Char) ] = join $ map zipRow l2
  pure $ Set.fromList $ map fst $ filter isAlive l3
  where
    zipRow :: (Int, String) -> [ (Coords, Char) ]
    zipRow (y, str) = [ ((x, y), ch) | (x, ch) <- zip [1..] str ]
    isAlive (_, ch) = ch == 'x'


golSimulator :: GameState -> IO GameState
golSimulator st@(GameState {aliveCellsVar}) = do
  atomically $ do
    lvl <- readTVar aliveCellsVar
    writeTVar aliveCellsVar $ golStep lvl
  pure st

startSimulation
  :: (GameState -> IO GameState)
  -> GameState
  -> Display
  -> IO ()
startSimulation simulator st glossWindow =
  simulateIO glossWindow black 2 st glossRenderer (\_ _ -> simulator)


main :: IO ()
main = do

  aliveCells <- loadBoard "./data/gol_board.txt"

  (st, glossWindow) <- initGame
    defaultGlossWindowSize
    defaultGlossWindowPosition
    defaultGridDimensions
    defaultBareCellSize
    defaultCellSpaceSize
    aliveCells
    defaultDbgOptions

  startSimulation golSimulator st glossWindow
