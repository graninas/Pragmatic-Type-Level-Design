{-# LANGUAGE DataKinds #-}
module Seeds where

import Automaton ( CellWorld )

import qualified Data.Map as Map


type SeedsRule = "Seeds"       -- DataKinds used here
type Seeds = CellWorld SeedsRule

{-
-- Alternative:

type Seeds = CellWorld "Seeds"
-}

-- TODO: rules

seedsStep :: Seeds -> Seeds
seedsStep = error "Not implemented"
