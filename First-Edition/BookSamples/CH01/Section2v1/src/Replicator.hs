module Replicator where

import Board ( loadFromFile, saveToFile, Board )

import qualified Data.Map as Map


newtype Replicator = Replicator Board
  deriving (Show, Eq)

-- TODO: rules

replicatorStep :: Replicator -> Replicator
replicatorStep = error "Not implemented"

iterateReplicator :: Int -> Replicator -> Replicator
iterateReplicator n replicator | n == 0 = replicator
iterateReplicator n replicator | n > 0 =
  head (drop 5 (iterate replicatorStep replicator))
iterateReplicator _ _ = error "Invalid iteration count"

loadReplicatorFromFile :: FilePath -> IO Replicator
loadReplicatorFromFile path = do
  (board :: Board) <- loadFromFile path
  pure (Replicator board)

saveReplicatorToFile :: FilePath -> Replicator -> IO ()
saveReplicatorToFile path (Replicator board) =
  saveToFile path board

