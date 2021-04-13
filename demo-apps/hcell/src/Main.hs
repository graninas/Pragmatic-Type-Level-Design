module Main where

import HCell.Prelude

import HCell.Types
import HCell.Gloss.Types
import HCell.Gloss.Conv
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
  -> Level
  -> DebugOptions
  -> IO (GameState, Display)
initGame
  (GlossWindowSize wndSize)
  (GlossWindowPosition wndPos)
  gridDims
  bareCellSize
  cellSpaceSize
  level
  dbgOpts = do
    let glossWindow = InWindow "Cellular automata" wndSize wndPos
    st <- GameState
      <$> newTVarIO (GlossWindowSize wndSize)
      <*> newTVarIO gridDims
      <*> newTVarIO bareCellSize
      <*> newTVarIO cellSpaceSize
      <*> newTVarIO level
      <*> newTVarIO dbgOpts
    pure (st, glossWindow)


loadLevel :: String -> IO Level
loadLevel lvlFileName = do
  l1 :: [String] <- (reverse . map T.unpack . lines) <$> (readFile lvlFileName)
  let l2 :: [(Int, String)] = zip [1..] l1
  let l3 :: [ (Coords, Char) ] = join $ map zipRow l2
  pure $ Set.fromList $ map fst $ filter isAlive l3
  where
    zipRow :: (Int, String) -> [ (Coords, Char) ]
    zipRow (y, str) = [ ((x, y), ch) | (x, ch) <- zip [1..] str ]
    isAlive (pos, ch) = ch == 'x'


simulator :: viewport -> Float -> GameState -> IO GameState
simulator _ _ st@(GameState {levelVar}) = do
  atomically $ do
    lvl <- readTVar levelVar
    writeTVar levelVar $ golStep lvl
  pure st

main :: IO ()
main = do

  lvl <- loadLevel "./data/lvl.txt"

  (st, glossWindow) <- initGame
    defaultGlossWindowSize
    defaultGlossWindowPosition
    defaultGridDimensions
    defaultBareCellSize
    defaultCellSpaceSize
    lvl
    defaultDbgOptions

  simulateIO glossWindow black 2 st glossRenderer simulator
