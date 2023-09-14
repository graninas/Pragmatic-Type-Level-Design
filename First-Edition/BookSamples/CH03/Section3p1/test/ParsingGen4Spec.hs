{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module ParsingGen4Spec where

import Cell
import Board
import Automaton
import Automata.GameOfLife

import Control.Monad
import Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import Data.Constraint.Symbol
import GHC.TypeLits
import Test.Hspec


data CustomChar where
  CH :: KnownSymbol s => Proxy s -> CustomChar

data CustomSymbol where
  CS :: [CustomChar] -> CustomSymbol

showCH :: CustomChar -> String
showCH (CH proxy) = symbolVal proxy

showCS :: CustomSymbol -> String
showCS (CS chs) = join $ map showCH chs

mergeCS :: CustomSymbol -> CustomSymbol -> CustomSymbol
mergeCS (CS chs1) (CS chs2) = CS (chs1 <> chs2)

appendCH :: CustomSymbol -> CustomChar -> CustomSymbol
appendCH (CS chs) ch = CS (chs <> [ch])


type Parser = String -> Maybe (CustomSymbol, String)

parseCustomChar :: String -> (Char, CustomChar) -> Maybe (CustomChar, String)
parseCustomChar (a:rest) (a', ch) | a == a' = Just (ch, rest)
parseCustomChar _ _ = Nothing

parseB :: String -> Maybe (CustomSymbol, String)
parseB str = let
  mbParsed = parseCustomChar str ('B', CH (Proxy @"B"))
  in case mbParsed of
    Nothing -> Nothing
    Just (ch, rest) -> Just (CS [ch], rest)

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

parseNumber :: String -> Maybe (CustomSymbol, String)
parseNumber [] = Nothing
parseNumber (a:rest) = case Map.lookup a numbers of
  Nothing -> Nothing
  Just ch -> Just (CS [ch], rest)

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

sequenceParsers :: [Parser] -> Parser
sequenceParsers [] str = Nothing
sequenceParsers (p:ps) str = case p str of
  Nothing -> Nothing
  Just (CS chs1, rest1) -> case sequenceParsers ps rest1 of
    Nothing -> Just (CS chs1, rest1)
    Just (CS chs2, rest2) -> Just (CS (chs1 <> chs2), rest2)

myParsers :: [Parser]
myParsers = [parseB, many1 parseNumber]

parse :: Parser -> String -> Maybe (CustomSymbol, String)
parse p = p

mergePSymbols
  :: forall s1 s2
   . (KnownSymbol s1, KnownSymbol s2)
  => Proxy (s1 :: Symbol)
  -> Proxy (s2 :: Symbol)
  -> Proxy (AppendSymbol s1 s2)
mergePSymbols _ _ = Proxy :: Proxy (AppendSymbol s1 s2)

printPSymbol :: KnownSymbol s => Proxy (s :: Symbol) -> String
printPSymbol = symbolVal


spec :: Spec
spec =
  describe "Parsing gen tests" $ do
    it "Test1" $ do

      let mbParsed = parse (sequenceParsers myParsers) "B12"

      case mbParsed of
        Nothing -> print "not parsed"
        Just (cs, rest) -> do
          print $ showCS cs
          print rest

      -- let (mbCs1, rest1) = parseB (Just (CS []), "B12")
      -- let (mbCs2, rest2) = parse1 (mbCs1, rest1)

      -- let (mbP1, rest1') = parseNumber (Just (CS []), "23")
      -- let (mbP2, rest2') = parseNumber (mbP1, rest1')

      -- let mbP12 = mergePSymbols <$> mbP1 <*> mbP2

      -- print $ showCS <$> mbCs1
      -- print $ showCS <$> mbCs2
      -- print rest1
      -- print rest2

      -- print $ printPSymbol <$> mbP1
      -- print $ printPSymbol <$> mbP2
      -- print $ printPSymbol <$> mbP12

