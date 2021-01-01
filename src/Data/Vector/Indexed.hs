-- Some imaginary implementation of the indexed vector.

{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Data.Vector.Indexed where

import GHC.TypeNats

data Vector (n :: Nat) a
  deriving Show


concat
  :: Vector (n :: Nat) a
  -> Vector (m :: Nat) a
  -> Vector (n + m) a
concat = error "Not implemented yet."


fromString' :: String -> Maybe (Vector n Char)
fromString' = error "Not implemented yet."



data ParseError
  = EmptyString
  | OutOfBoundaries


fromString :: String -> Either ParseError (Vector n Char)
fromString = error "Not implemented yet."
