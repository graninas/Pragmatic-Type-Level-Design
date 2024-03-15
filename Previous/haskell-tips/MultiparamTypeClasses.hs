{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiparamTypeClasses where
import Data.Data (Proxy)


-- This snippet demonstrates the sensitivity of multiparam
-- type classes to the order of parameters.


data Type1 where
  Type1Def :: Type1

data Type2 (type1 :: Type1) where
  Type2Def :: Type2 board

-- Swapping type1 and type2 leads to a compilation error:
class Class type1 type2 where
  doSmth
    :: Proxy (type2 :: Type2 type1)
    -> Proxy (type1 :: Type1)
    -> Proxy ()


main :: IO ()
main = pure ()
