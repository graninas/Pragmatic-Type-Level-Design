-- imaginary-indexed-vector library.
-- Not an actual implementation, just a sample of a high-level interface.

{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Vector.Indexed where

import GHC.TypeNats
import Data.Proxy

data Vector (n :: Nat) a
  deriving Show


merge
  :: forall n m z a
   . z ~ (n + m)
  => Vector (n :: Nat) a
  -> Vector (m :: Nat) a
  -> Vector (z :: Nat) a
merge = error "Not implemented yet."

sortAsc
  :: Ord a
  => Vector (n :: Nat) a
  -> Vector (n :: Nat) a
sortAsc = error "Not implemented yet."

splitAt
  :: forall n idx z a
   . z ~ (n - idx)
  => (idx <=? n) ~ 'True
  => Proxy (idx :: Nat)
  -> Vector (n :: Nat) a
  -> (Vector (idx :: Nat) a, Vector (z :: Nat) a)
splitAt = error "Not implemented yet."



data ParseError
  = EmptyString
  | OutOfBoundaries


fromString :: String -> Either ParseError (Vector n Char)
fromString = error "Not implemented yet."
