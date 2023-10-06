{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module ParsingGen3Spec where

import Test.Hspec
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map

import Cell
import Board
import Automaton
import GHC.TypeLits


-- Value-level parsing with custom Num types.
--   Less boilerplate than in ParsingGen2.


data S
data B

data P = P Integer


data CustomSRule = CustomSRule Integer
  deriving (Show, Eq, Ord)

data CustomBRule = CustomBRule Integer
  deriving (Show, Eq, Ord)

data CustomRule = CustomRule [CustomBRule] [CustomSRule]
  deriving (Show, Eq, Ord)



parseRule :: String -> CustomRule
parseRule str = parseRule' str (CustomRule [] [])


parseRule' :: String -> CustomRule -> CustomRule
parseRule' str1 (CustomRule bs ss) = case (str1, bs, ss) of
  ('S':str2, _, []) ->
    let (str3, sRules) = parseSRule (str2, [])
    in parseRule' str3 (CustomRule bs sRules)
  ('S':_, _, _) -> error "Custom S Rule is already provided"

  ('B':str2, [], _) ->
    let (str3, bRules) = parseBRule (str2, [])
    in parseRule' str3 (CustomRule bRules ss)
  ('B':_, _, _) -> error "Custom B Rule is already provided"

  ([], _, _) -> CustomRule bs ss

  (' ':str2, _, _) -> parseRule' str2 (CustomRule bs ss)

  (n, _, _) -> error ("parseRule not supported: " <> n)


numbers :: [(Char, P)]
numbers =
  [ ('0', P 0)
  , ('1', P 1)
  , ('2', P 2)
  , ('3', P 3)
  , ('4', P 4)
  , ('5', P 5)
  , ('6', P 6)
  , ('7', P 7)
  , ('8', P 8)
  ]



parseSRule :: (String, [CustomSRule]) -> (String, [CustomSRule])
parseSRule ([], sRules) = ([], sRules)
parseSRule (' ':str1, sRules) = (str1, sRules)
parseSRule (ch:str1, sRules) =
  case lookup ch numbers of
    Nothing -> error ("parseSRule not supported: " <> (ch:str1))
    Just (P n) -> let
      sRule = CustomSRule n
      in parseSRule (str1, sRule : sRules)


parseBRule :: (String, [CustomBRule]) -> (String, [CustomBRule])
parseBRule ([], bRules) = ([], bRules)
parseBRule (' ':str1, bRules) = (str1, bRules)
parseBRule (ch:str1, bRules) =
  case lookup ch numbers of
    Nothing -> error ("parseBRule not supported: " <> (ch:str1))
    Just (P n) -> let
      bRule = CustomBRule n
      in parseBRule (str1, bRule : bRules)


runCustomRule :: CustomRule -> Board -> Board
runCustomRule (CustomRule bRules sRules) board = board'
  where
    updateCell :: Coords -> Cell
    updateCell pos = let
      alive = fromIntegral $ countAliveNeighbours board pos
      state = Map.lookup pos board
      sRuleWorked = any (\(CustomSRule n) -> n == alive) sRules
      bRuleWorked = any (\(CustomBRule n) -> n == alive) bRules
      in case (state, sRuleWorked, bRuleWorked) of
        (Just Alive, True, _) -> Alive
        (Just Dead, _, True) -> Alive
        (Nothing, _, True) -> Alive
        _ -> Dead
    board' :: Board
    board' = Map.mapWithKey (\pos _ -> updateCell pos) board


glider :: Board
glider = fillBoard (-1, -1) (3, 3)
  $ Map.fromList [((1, 0), Alive),
                 ((2, 1), Alive),
                 ((0, 2), Alive),
                 ((1, 2), Alive),
                 ((2, 2), Alive)]


glider2Expected :: Board
glider2Expected = Map.fromList
    [((-1,-1),Dead),((-1,0),Dead),((-1,1),Dead)
    ,((-1,2),Dead),((-1,3),Dead),((0,-1),Dead)
    ,((0,0),Dead),((0,1),Alive),((0,2),Dead)
    ,((0,3),Dead),((1,-1),Dead),((1,0),Dead)
    ,((1,1),Dead),((1,2),Alive),((1,3),Alive)
    ,((2,-1),Dead),((2,0),Dead),((2,1),Alive)
    ,((2,2),Alive),((2,3),Dead),((3,-1),Dead)
    ,((3,0),Dead),((3,1),Dead),((3,2),Dead),((3,3),Dead)]


spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do
      let r = parseRule "S23 B3"
      let board2 = runCustomRule r glider
      board2 `shouldBe` glider2Expected



