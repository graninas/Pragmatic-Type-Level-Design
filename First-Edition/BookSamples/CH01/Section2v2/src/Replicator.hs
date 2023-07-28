module Replicator where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton ( Automaton(..) )

import qualified Data.Map as Map


newtype Replicator = Replicator Board
  deriving (Show, Eq)

instance Automaton Replicator where
  step :: Replicator -> Replicator              -- N.B., InstanceSigs is enabled to show sigs
  step = replicatorStep
  loadFromFile :: FilePath -> IO Replicator
  loadFromFile = loadReplicatorFromFile
  saveToFile :: FilePath -> Replicator -> IO ()
  saveToFile = saveReplicatorToFile

-- TODO: rules

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"

loadReplicatorFromFile :: FilePath -> IO Replicator
loadReplicatorFromFile path = do
  (board :: Board) <- loadBoardFromFile path
  pure (Replicator board)

saveReplicatorToFile :: FilePath -> Replicator -> IO ()
saveReplicatorToFile path (Replicator board) =
  saveBoardToFile path board

