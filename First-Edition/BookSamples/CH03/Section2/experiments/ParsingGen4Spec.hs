{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module ParsingGen4Spec where

import Domain.Cell
import Domain.Board
import Domain.Automaton
import Assets.Automata.GameOfLife

import Control.Monad
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.Constraint.Symbol
import GHC.TypeLits
import Test.Hspec



-- Value-level parsing powered with existentials
data CustomChar where
  CH :: KnownSymbol s => Proxy s -> CustomChar

data CustomSymbol where
  CS :: [CustomChar] -> CustomSymbol



mergePSymbols
  :: forall s1 s2
   . (KnownSymbol s1, KnownSymbol s2)
  => Proxy (s1 :: Symbol)
  -> Proxy (s2 :: Symbol)
  -> Proxy (AppendSymbol s1 s2)
mergePSymbols _ _ = Proxy :: Proxy (AppendSymbol s1 s2)


mergeCChars :: [CustomChar] -> ()
mergeCChars [] = ()
mergeCChars (CH proxy1 : []) = ()
mergeCChars (CH proxy1 : CH proxy2 : rest) = let
  proxyA = mergePSymbols proxy1 proxy2
  -- ch = toCH proxyA
  x = mergeCChars rest
  in ()

-- toCH :: forall s. KnownSymbol s => Proxy s -> CustomChar
toCH proxy = CH proxy


showCH :: CustomChar -> String
showCH (CH proxy) = symbolVal proxy

showCS :: CustomSymbol -> String
showCS (CS chs) = join (map showCH chs)

mergeCS :: CustomSymbol -> CustomSymbol -> CustomSymbol
mergeCS (CS chs1) (CS chs2) = CS (chs1 <> chs2)

appendCH :: CustomSymbol -> CustomChar -> CustomSymbol
appendCH (CS chs) ch = CS (chs <> [ch])


type Parser = String -> Maybe (CustomSymbol, String)

customChar :: (Char, CustomChar) -> String -> Maybe (CustomChar, String)
customChar (ch', sch) (ch:rest) | ch == ch' = Just (sch, rest)
customChar _ _ = Nothing


char
 :: KnownSymbol s
 => Proxy s
 -> String
 -> Maybe (CustomChar, String)
char proxy = customChar (head (symbolVal proxy), CH proxy)

asString
 :: (String -> Maybe (CustomChar, String))
 -> String
 -> Maybe (CustomSymbol, String)
asString p str = case p str of
  Nothing -> Nothing
  Just (ch, rest) -> Just (CS [ch], rest)

many :: Parser -> String -> Maybe (CustomSymbol, String)
many p [] = Nothing
many p str = case p str of
  Nothing -> Nothing
  Just (CS chs1, rest1) -> case many p rest1 of
    Nothing -> Just (CS chs1, rest1)
    Just (CS chs2, rest2) -> Just (CS (chs1 <> chs2), rest2)

many1 :: Parser -> String -> Maybe (CustomSymbol, String)
many1 p str = case p str of
  Nothing -> error "Must be at least 1 symbol"
  Just (CS chs1, rest1) ->
    case many p rest1 of
      Nothing -> Just (CS chs1, rest1)
      Just (CS chs2, rest2) -> Just (CS (chs1 <> chs2), rest2)

sequenced :: [Parser] -> Parser
sequenced [] str = Nothing
sequenced (p:ps) str = case p str of
  Nothing -> Nothing
  Just (CS chs1, rest1) -> case sequenced ps rest1 of
    Nothing -> Just (CS chs1, rest1)
    Just (CS chs2, rest2) -> Just (CS (chs1 <> chs2), rest2)

parse :: Parser -> String -> Maybe (CustomSymbol, String)
parse p = p






numbers :: Map.Map Char CustomChar
numbers = Map.fromList
  [ ('1', CH (Proxy @"1"))
  , ('2', CH (Proxy @"2"))
  , ('3', CH (Proxy @"3"))
  , ('4', CH (Proxy @"4"))
  , ('5', CH (Proxy @"5"))
  , ('6', CH (Proxy @"6"))
  , ('7', CH (Proxy @"7"))
  , ('8', CH (Proxy @"8"))
  , ('9', CH (Proxy @"9"))
  ]

number :: String -> Maybe (CustomSymbol, String)
number [] = Nothing
number (a:rest) = case Map.lookup a numbers of
  Nothing -> Nothing
  Just ch -> Just (CS [ch], rest)




bChar :: Parser
bChar = asString (customChar ('B', CH (Proxy @"B")))

slashToken :: Parser
slashToken = asString (customChar ('/', CH (Proxy @"/")))

bToken :: Parser
bToken = sequenced [bChar, many1 number]

sToken :: Parser
sToken = sequenced [asString (char (Proxy @"S")), many1 number]

customRule :: Parser
customRule = sequenced [bToken, slashToken, sToken]


spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      let mbBToken = parse bToken "B12----"
      let mbInvalidSToken = parse sToken "  S12"
      let mbSToken = parse sToken "S12+++"
      let mbRule = parse customRule "B135/S135===="

      case mbBToken of
        Nothing -> error "B token not parsed"
        Just (cs, rest) -> do
          showCS cs `shouldBe` "B12"
          rest `shouldBe` "----"

      case mbInvalidSToken of
        Nothing -> pure ()
        Just (cs, rest) -> error "Invalid S token is parsed"

      case mbSToken of
        Nothing -> error "S token not parsed"
        Just (cs, rest) -> do
          showCS cs `shouldBe` "S12"
          rest `shouldBe` "+++"

      case mbRule of
        Nothing -> error "Rule not parsed"
        Just (cs, rest) -> do
          showCS cs `shouldBe` "B135/S135"
          rest `shouldBe` "===="


