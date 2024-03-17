{-# LANGUAGE DataKinds #-}

module Lib.TypeSelector where



-- | Granular TypeSelector design pattern.

data Level
  = TypeLevel
  | ValueLevel

type family StringType (lvl :: Level) where
  StringType 'TypeLevel  = Symbol
  StringType 'ValueLevel = String

type family IntegerType (lvl :: Level) where
  IntegerType 'TypeLevel  = Nat
  IntegerType 'ValueLevel = Int
