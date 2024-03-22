{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.TypeSelector where

import GHC.TypeLits


-- | Granular TypeSelector design pattern.

data Level
  = TypeLevel
  | ValueLevel

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

type family IntType (lvl :: Level) where
  IntType 'TypeLevel  = Nat
  IntType 'ValueLevel = Int

type family CharType (lvl :: Level) where
  CharType 'TypeLevel  = Symbol
  CharType 'ValueLevel = Char
