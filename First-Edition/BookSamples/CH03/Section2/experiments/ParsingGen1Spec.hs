{-# LANGUAGE TypeApplications #-}
module ParsingGen1Spec where

import Test.Hspec
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map

import Domain.Cell
import Domain.Board
import Domain.Automaton


-- Value-level verbose parsing with custom Num types

data S
data B
data Num1
data Num2
data Num3
data Num4
data Num5
data Num6
data Num7
data Num8




class RulePart rulePart where
  getNumber :: Proxy rulePart -> Int


instance RulePart S where
  getNumber _ = error "not supported for S"

instance RulePart Num1 where
  getNumber _ = 1

instance RulePart Num2 where
  getNumber _ = 2

instance RulePart Num3 where
  getNumber _ = 3

instance RulePart Num4 where
  getNumber _ = 4

instance RulePart Num5 where
  getNumber _ = 5

instance RulePart Num6 where
  getNumber _ = 6

instance RulePart Num7 where
  getNumber _ = 7

instance RulePart Num8 where
  getNumber _ = 8


data CustomSRule = CustomSRule Int
  deriving (Show, Eq, Ord)

data CustomBRule = CustomBRule Int
  deriving (Show, Eq, Ord)

data CustomRule = CustomRule [CustomBRule] [CustomSRule]
  deriving (Show, Eq, Ord)



parseRule :: String -> CustomRule
parseRule str = parseRule' str (CustomRule [] [])


parseRule' :: String -> CustomRule -> CustomRule
parseRule' [] rule = rule
parseRule' str1 (CustomRule bs ss) = case (str1, bs, ss) of
  ('S':str2, _, []) ->
    let (str3, sRules) = parseSRule (str2, [])
    in parseRule' str3 (CustomRule bs sRules)
  ('S':_, _, _) -> error "Custom S Rule is already provided"

  ('B':str2, [], _) ->
    let (str3, bRules) = parseBRule (str2, [])
    in parseRule' str3 (CustomRule bRules ss)
  ('B':_, _, _) -> error "Custom B Rule is already provided"

  (' ':str2, _, _) -> parseRule' str2 (CustomRule bs ss)

  (n, _, _) -> error ("parseRule not supported: " <> n)


parseSRule :: (String, [CustomSRule]) -> (String, [CustomSRule])
parseSRule ([], sRules) = ([], sRules)
parseSRule (' ':str2, sRules) = (str2, sRules)
parseSRule (str1, sRules) = case str1 of
  '1':str2 -> let
    sRule = constructSRule (Proxy @Num1)
    in parseSRule (str2, sRule : sRules)
  '2':str2 -> let
    sRule = constructSRule (Proxy @Num2)
    in parseSRule (str2, sRule : sRules)
  '3':str2 -> let
    sRule = constructSRule (Proxy @Num3)
    in parseSRule (str2, sRule : sRules)
  '4':str2 -> let
    sRule = constructSRule (Proxy @Num4)
    in parseSRule (str2, sRule : sRules)
  '5':str2 -> let
    sRule = constructSRule (Proxy @Num5)
    in parseSRule (str2, sRule : sRules)
  '6':str2 -> let
    sRule = constructSRule (Proxy @Num6)
    in parseSRule (str2, sRule : sRules)
  '7':str2 -> let
    sRule = constructSRule (Proxy @Num7)
    in parseSRule (str2, sRule : sRules)
  '8':str2 -> let
    sRule = constructSRule (Proxy @Num8)
    in parseSRule (str2, sRule : sRules)
  n -> error ("parseSRule not supported: " <> n)

constructSRule :: RulePart s => Proxy s -> CustomSRule
constructSRule proxy = CustomSRule (getNumber proxy)



parseBRule :: (String, [CustomBRule]) -> (String, [CustomBRule])
parseBRule ([], bRules) = ([], bRules)
parseBRule (' ':str2, bRules) = (str2, bRules)
parseBRule (str1, bRules) = case str1 of
  '1':str2 -> let
    bRule = constructBRule (Proxy @Num1)
    in parseBRule (str2, bRule : bRules)
  '2':str2 -> let
    bRule = constructBRule (Proxy @Num2)
    in parseBRule (str2, bRule : bRules)
  '3':str2 -> let
    bRule = constructBRule (Proxy @Num3)
    in parseBRule (str2, bRule : bRules)
  '4':str2 -> let
    bRule = constructBRule (Proxy @Num4)
    in parseBRule (str2, bRule : bRules)
  '5':str2 -> let
    bRule = constructBRule (Proxy @Num5)
    in parseBRule (str2, bRule : bRules)
  '6':str2 -> let
    bRule = constructBRule (Proxy @Num6)
    in parseBRule (str2, bRule : bRules)
  '7':str2 -> let
    bRule = constructBRule (Proxy @Num7)
    in parseBRule (str2, bRule : bRules)
  '8':str2 -> let
    bRule = constructBRule (Proxy @Num8)
    in parseBRule (str2, bRule : bRules)
  n -> error ("parseBRule not supported: " <> n)

constructBRule :: RulePart s => Proxy s -> CustomBRule
constructBRule proxy = CustomBRule (getNumber proxy)





runCustomRule :: CustomRule -> Board -> Board
runCustomRule (CustomRule bRules sRules) board = board'
  where
    updateCell :: Coords -> Cell
    updateCell pos = let
      alive = countAliveNeighbours board pos
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



