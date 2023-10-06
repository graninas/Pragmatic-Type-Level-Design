module Seeds where

import Board ( Board, loadFromFile, saveToFile )

import qualified Data.Map as Map


newtype Seeds = Seeds Board
  deriving (Show, Eq)

-- TODO: rules

seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"

iterateSeeds :: Int -> Seeds -> Seeds
iterateSeeds n seeds | n == 0 = seeds
iterateSeeds n seeds | n > 0 =
  head (drop 5 (iterate seedsStep seeds))
iterateSeeds _ _ = error "Invalid iteration count"

loadSeedsFromFile :: FilePath -> IO Seeds
loadSeedsFromFile path = do
  (board :: Board) <- loadFromFile path
  pure (Seeds board)

saveSeedsToFile :: FilePath -> Seeds -> IO ()
saveSeedsToFile path (Seeds board) =
  saveToFile path board

