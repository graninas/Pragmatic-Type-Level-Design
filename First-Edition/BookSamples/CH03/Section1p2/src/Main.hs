{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Proxy
import GHC.TypeLits

class Description a where
  describe :: Proxy a -> String


data Benoit
data Mandelbrot name

type Fractal = Mandelbrot (Mandelbrot (Mandelbrot Benoit))

instance Description Benoit where
  describe _ = "Benoit"

instance
  Description n =>
  Description (Mandelbrot n) where
  describe _ = "Mandelbrot "
    <> describe (Proxy @n)


data Person (firstName :: Symbol) (lastName :: Symbol)

instance
  (KnownSymbol fn, KnownSymbol ln) =>
  Description (Person fn ln) where
  describe _ =
    symbolVal (Proxy @fn)
    <> " "
    <> symbolVal (Proxy @ln)

main :: IO ()
main = do
  print (describe (Proxy @Fractal))
  print (describe (Proxy @(Person "Benoit" "Mandelbrot")))


