{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Seeds where

import Board ( loadBoardFromFile, saveBoardToFile, Board )
import Automaton

import qualified Data.Map as Map


type SeedsRule = "Seeds"       -- DataKinds used here
type Seeds = CellWorld SeedsRule

{-
-- Alternative:

type Seeds = CellWorld "Seeds"
-}

instance Automaton SeedsRule where  -- FlexibleInstances used here
  step :: Seeds -> Seeds            -- InstanceSigs is enabled to show sigs
  step = seedsStep
  code _ = "seeds"


-- TODO: rules

seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"
