{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.HasValue where

class HasValue ctx valName valType | ctx valName -> valType where
  getVal :: ctx -> valType
