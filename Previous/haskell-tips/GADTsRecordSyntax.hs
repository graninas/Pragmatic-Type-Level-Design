{-# LANGUAGE GADTs #-}
module GADTsRecordSyntax where

import GHC.TypeLits ( Symbol )


-- This snippet demonstrates how field names can be used
-- with Haskell's GADTs syntax.


data MyGADT a where
  MkMyGADT :: { name :: String } -> MyGADT String

data Term a where
  Lit :: { val :: Int } -> Term Int

