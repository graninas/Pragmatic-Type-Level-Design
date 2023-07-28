module Seeds where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype Seeds = Seeds Board
  deriving (Show, Eq)

instance Automaton Seeds where
  step :: Seeds -> Seeds                      -- N.B., InstanceSigs is enabled to show sigs
  step = seedsStep
  loadFromFile :: FilePath -> IO Seeds
  loadFromFile = loadSeedsFromFile
  saveToFile :: FilePath -> Seeds -> IO ()
  saveToFile = saveSeedsToFile


-- TODO: rules

seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"

loadSeedsFromFile :: FilePath -> IO Seeds
loadSeedsFromFile path = do
  (board :: Board) <- loadBoardFromFile path
  pure (Seeds board)

saveSeedsToFile :: FilePath -> Seeds -> IO ()
saveSeedsToFile path (Seeds board) =
  saveBoardToFile path board

