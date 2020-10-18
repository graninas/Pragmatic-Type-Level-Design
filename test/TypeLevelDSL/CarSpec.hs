{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.CarSpec where

import TypeLevelDSL.Car.Language
import TypeLevelDSL.Car.Implementation
import TypeLevelDSL.Eval

import           Test.Hspec
import           Data.Proxy (Proxy(..))

-- Extensions

data FusionMkI
data BrokenEngine

-- Implementation

instance Eval AsEngine FusionMkI () where
  eval _ _ = putStrLn "Engine: FusionMkI"

instance Eval AsEngine BrokenEngine () where
  eval _ _ = putStrLn "Engine: BrokenEngine"

instance Eval AsPart FusionMkI () where
  eval _ _ = putStrLn "Part: FusionMkI"

instance Eval AsPart BrokenEngine () where
  eval _ _ = putStrLn "Part: BrokenEngine"

-- Scripts

type MyCar1 = Car "A" (Engine FusionMkI) (Parts '[])
type MyCar2 = Car "B" (Engine FusionMkI) (Parts '[FusionMkI])
type MyCar3 = Car "C" (Engine FusionMkI) (Parts '[FusionMkI, BrokenEngine])

runner :: IO ()
runner = do
  eval AsEngine (Proxy :: Proxy (Engine FusionMkI))
  eval AsEngine (Proxy :: Proxy (Engine BrokenEngine))
  eval AsPart (Proxy :: Proxy (Parts '[]))
  eval AsPart (Proxy :: Proxy (Parts '[FusionMkI]))
  eval AsPart (Proxy :: Proxy (Parts '[FusionMkI, BrokenEngine]))
  eval AsCar (Proxy :: Proxy MyCar3)

-- Output:
-- Engine: FusionMkI
-- Engine: BrokenEngine
-- Part: FusionMkI
-- Part: FusionMkI
-- Part: BrokenEngine
-- This is a car.
-- Engine: FusionMkI
-- Part: FusionMkI
-- Part: BrokenEngine

spec :: Spec
spec =
  describe "Type level Servant-like eDSL Car" $ do
    it "Run Car script" $ do
      runner
