{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE GADTs                    #-}

module Tests.EmptyADTsModelSpec where

import CPrelude

import qualified Prelude

import Test.Hspec
import GHC.TypeLits


-- Incoherent model

data Benoit
data Mandelbrot (name :: *)

type Fractal = Mandelbrot (Mandelbrot (Mandelbrot Benoit))

-- Invalid value compiles:
type WhatAmI = Mandelbrot Bool



-- Coherent model

data PersonType = Person
  { firstName :: Symbol
  , lastName :: Symbol
  }
data User
  (login :: Symbol)
  (person :: PersonType)

type MandelbrotPerson = Person "Benoit" "Mandelbrot"
type MandelbrotUser   = User "mandel" MandelbrotPerson

-- Invalid value won't compile:
-- type WhatAmI = User "invalid user" Bool



spec :: Spec
spec = do
  describe "Empty parametrized ADTs model test" $ do

    it "Dummy" $ do
      1 `shouldBe` 1


