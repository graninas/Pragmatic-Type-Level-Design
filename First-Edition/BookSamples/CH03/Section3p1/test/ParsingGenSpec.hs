{-# LANGUAGE TypeApplications #-}
module ParsingGenSpec where

import Test.Hspec
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map

import Cell
import Board
import Automaton


data S


data CustomRule = CustomRule String
  deriving (Show, Eq, Ord)

parseRule :: String -> [CustomSRule]
parseRule str1 = case str1 of
  'S':str2 -> parseSRule str2 []
  n -> error ("parseRule not supported: " <> n)


data S1
data S3
data S5


data CustomSRule = CustomSRule Int
  deriving (Show, Eq, Ord)

parseSRule :: String -> [CustomSRule] -> [CustomSRule]
parseSRule str1 sRules = case str1 of
  [] -> sRules
  '1':str2 -> let
    sRule = constructSRule (Proxy @S1)
    in parseSRule str2 (sRule : sRules)
  '3':str2 -> let
    sRule = constructSRule (Proxy @S3)
    in parseSRule str2 (sRule : sRules)
  '5':str2 -> let
    sRule = constructSRule (Proxy @S5)
    in parseSRule str2 (sRule : sRules)
  n -> error ("parseSRule not supported: " <> n)

constructSRule :: RulePart s => Proxy s -> CustomSRule
constructSRule proxy = CustomSRule (getNumber proxy)




class RulePart rulePart where
  getLiteral :: Proxy rulePart -> String
  getNumber :: Proxy rulePart -> Int


instance RulePart S where
  getLiteral _ = "S"
  getNumber _ = error "not supported for S"

instance RulePart S1 where
  getLiteral _ = error "not supported for S"
  getNumber _ = 1

instance RulePart S3 where
  getLiteral _ = error "not supported for S"
  getNumber _ = 3

instance RulePart S5 where
  getLiteral _ = error "not supported for S"
  getNumber _ = 5


constructRule :: RulePart rulePart => Proxy rulePart -> CustomRule
constructRule proxy = CustomRule (getLiteral proxy)



runCustomSRule :: [CustomSRule] -> Board -> Board
runCustomSRule [] board = board
runCustomSRule sRules board = board'
  where
    updateCell :: Coords -> Cell
    updateCell pos = let
      alive = countAliveNeighbours board pos
      state = Map.lookup pos board
      sRuleWorked = any (\(CustomSRule n) -> n == alive) sRules
      in if sRuleWorked then Alive else Dead
    board' :: Board
    board' = Map.mapWithKey (\pos _ -> updateCell pos) board




glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do
      let r = parseRule "S135"
      print r

      let board2 = runCustomSRule r glider
      print board2



