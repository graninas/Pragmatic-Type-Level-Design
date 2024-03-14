module AutomataSpec where

import Cellular.Domain.Cell
import Cellular.Domain.Board
import Cellular.Domain.Automaton
import Cellular.Assets.Automata.GameOfLife
import Cellular.App.Existential.Rules
import Cellular.App.Existential.Worlds

import Test.Hspec
import Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

gol1 :: GoL
gol1 = CW $ fillBoard (-1, -1) (3, 3) glider

gol2Expected :: GoL
gol2Expected =
  CW (Map.fromList
    [((-1,-1),Dead),((-1,0),Dead),((-1,1),Dead)
    ,((-1,2),Dead),((-1,3),Dead),((0,-1),Dead)
    ,((0,0),Dead),((0,1),Alive),((0,2),Dead)
    ,((0,3),Dead),((1,-1),Dead),((1,0),Dead)
    ,((1,1),Dead),((1,2),Alive),((1,3),Alive)
    ,((2,-1),Dead),((2,0),Dead),((2,1),Alive)
    ,((2,2),Alive),((2,3),Dead),((3,-1),Dead)
    ,((3,0),Dead),((3,1),Dead),((3,2),Dead),((3,3),Dead)])

spec :: Spec
spec =
  describe "Automata tests" $ do
    it "Game of Life logic" $ do
      let gol2 = step gol1
      gol2 `shouldBe` gol2Expected
