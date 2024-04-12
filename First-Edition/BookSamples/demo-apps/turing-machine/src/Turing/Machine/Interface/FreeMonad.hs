{-# LANGUAGE GADTs #-}

{- This module demonstrates Free monad as an interface-like
abstraction. -}

module Turing.Machine.Interface.FreeMonad
  ( MachineMethod (..)
  , Machine
  , runFM
  , nameFM
  ) where

import Turing.Machine.Language

import Control.Monad.Free


-- | Interface for Turing Machines based on Free monads.

data MachineMethod next where
  Run :: Tape -> (Either String Tape -> next) -> MachineMethod next
  Name :: (String -> next) -> MachineMethod next

type Machine a = Free MachineMethod a

instance Functor MachineMethod where
  fmap f (Run tape next) = Run tape (f . next)
  fmap f (Name next) = Name (f . next)

runFM :: Tape -> Machine (Either String Tape)
runFM tape = liftF (Run tape id)

nameFM :: Machine String
nameFM = liftF (Name id)
