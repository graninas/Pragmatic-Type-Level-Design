{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Domain.Automaton where

import Domain.Board ( Board, saveBoardToFile, loadBoardFromFile )

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


data CellWorld rule = CW Board

-- type RuleCode = String

-- data RuleType where
--   HardcodedRule :: param -> RuleType
--   CustomRule :: param -> RuleType
--   LifeLike :: bRule -> sRule -> RuleType


-- class Automaton (param :: RuleType) where
--   step :: Proxy param -> Board -> Board
--   code :: Proxy param -> RuleCode
--   name :: Proxy param -> String


-- data Lines3S

-- type GoLRule = "GoL Rule"

-- instance Automaton (HardcodedRule GoLRule) where
-- instance Automaton (CustomRule Lines3S) where

