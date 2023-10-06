{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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


type MyParser = (Maybe CustomSymbol, String)


parseB :: MyParser -> MyParser
parseB (Nothing, rest) = (Nothing, rest)
parseB (Just cs, 'B':rest) = (Just $ appendCH cs $ CH $ Proxy @"B", rest)
parseB (mbCs, rest) = (mbCs, rest)

parse1 :: MyParser -> MyParser
parse1 (Nothing, rest) = (Nothing, rest)
parse1 (Just cs, '1':rest) = (Just $ appendCH cs $ CH $ Proxy @"1", rest)
parse1 (mbCs, rest) = (mbCs, rest)

parse2 :: String -> (Maybe (Proxy "2"), String)
parse2 ('2':rest) = (Just $ Proxy @"2", rest)
parse2 rest = (Nothing, rest)

parse3 :: String -> (Maybe (Proxy "3"), String)
parse3 ('3':rest) = (Just $ Proxy @"3", rest)
parse3 rest = (Nothing, rest)


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
      let (mbCs1, rest1) = parseB (Just (CS []), "B12")
      let (mbCs2, rest2) = parse1 (mbCs1, rest1)

      let (mbP1, rest1') = parse2 "23"
      let (mbP2, rest2') = parse3 "3"

      let mbP12 = mergePSymbols <$> mbP1 <*> mbP2

      print $ showCS <$> mbCs1
      print $ showCS <$> mbCs2
      print rest1
      print rest2

      print $ printPSymbol <$> mbP1
      print $ printPSymbol <$> mbP2
      print $ printPSymbol <$> mbP12

