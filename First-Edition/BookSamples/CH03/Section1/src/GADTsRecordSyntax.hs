{-# LANGUAGE GADTs #-}
module GADTsRecordSyntax where

import GHC.TypeLits ( Symbol )

data MyGADT a where
  MkMyGADT :: { name :: String } -> MyGADT String

data Term a where
  Lit :: { val :: Int } -> Term Int

