{-# LANGUAGE TypeApplications #-}

module CH01_06_2Spec where

import Prelude

import           Data.Proxy (Proxy(..))
import           Data.IORef
import qualified Data.Map as Map
import Control.Exception (ErrorCall, SomeException, try)
import           Test.Hspec
import Data.Maybe (fromJust)





data Cell = Alive | Dead
  deriving (Show, Read, Eq)

type Board = Map.Map (Int, Int) Cell



gliderPattern :: [[Char]]
gliderPattern =
    [ ['X', 'X', 'X']
    , ['X', ' ', ' ']
    , [' ', 'X', ' ']
    ]

glider = fromLists gliderPattern

fromLists :: [[Char]] -> Board
fromLists pattern = board
  where
    (_, board) = foldr fromRows (0, Map.empty) pattern

fromRows cols (row, board) = (row+1, newBoard)
  where
    (_, newBoard) = foldr (fromColumns row) (0, board) cols

fromColumns row 'X' (col, board) = (col+1, Map.insert (row, col) Alive board)
fromColumns row _   (col, board) = (col+1, board)



spec :: Spec
spec =
  describe "Sample 01_6_2 test" $ do
    it "Test" $ do
      print $ Map.toList glider
      1 `shouldBe` 2
