module GameOfLife where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype GoL = GoL Board
  deriving (Show, Eq)

-- TODO: rules

instance Automaton GoL where
  step :: GoL -> GoL                      -- N.B., InstanceSigs is enabled to show sigs
  step = golStep
  loadFromFile :: FilePath -> IO GoL
  loadFromFile = loadGoLFromFile
  saveToFile :: FilePath -> GoL -> IO ()
  saveToFile = saveGoLToFile

golStep :: GoL -> GoL
golStep = error "Not implemented"

loadGoLFromFile :: FilePath -> IO GoL
loadGoLFromFile path = do
  (board :: Board) <- loadBoardFromFile path
  pure (GoL board)

saveGoLToFile :: FilePath -> GoL -> IO ()
saveGoLToFile path (GoL board) =
  saveBoardToFile path board

