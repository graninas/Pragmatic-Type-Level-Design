{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Proxy


data IAmEmptyADT             -- No value constructor available
data IAmAnotherEmptyADT      -- No value constructor available

emptyADTProxy :: Proxy IAmEmptyADT
emptyADTProxy = Proxy

anotherEmptyADTProxy :: Proxy IAmAnotherEmptyADT
anotherEmptyADTProxy = Proxy


class Description a where
  describe :: Proxy a -> String

instance Description IAmEmptyADT where
  describe _ = "IAmEmptyADT"

instance Description IAmAnotherEmptyADT where
  describe _ = "IAmAnotherEmptyADT"



data Board         -- some Board type

newtype CellWorld rule = CW Board

data GoLRule :: *
type GoL = CellWorld GoLRule



data Benoit
data Mandelbrot name

type Fractal = Mandelbrot (Mandelbrot (Mandelbrot Benoit))


instance Description Benoit where
  describe _ = "Benoit"

instance
  Description n =>
  Description (Mandelbrot n) where
  describe _ = "Mandelbrot "
    <> describe (Proxy :: Proxy n)





main :: IO ()
main = do
  print (describe emptyADTProxy)
  print (describe anotherEmptyADTProxy)
  print (describe (Proxy :: Proxy Fractal))

