{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Proxy
import GHC.TypeLits

class Description a where
  describe :: Proxy a -> String

data Person (firstName :: Symbol) (lastName :: Symbol)
data User (login :: Symbol) person

instance
  (KnownSymbol fn, KnownSymbol ln) =>
  Description (Person fn ln) where
  describe _ =
    symbolVal (Proxy @fn) <> " " <> symbolVal (Proxy @ln)


getUserDescription
  :: forall login pincode person
   . ( KnownSymbol login
     , KnownNat pincode
     , Description person
     )
  => Proxy (User login person)
  -> String
getUserDescription _
     = "User: " <> symbolVal (Proxy @login)
    <> ", pincode: " <> show (natVal (Proxy @pincode))
    <> ", Person: " <> describe (Proxy @person)

type MandelbrotUser = User "mandel" (Person "Benoit" "Mandelbrot")
type InvalidUser = User "invalid" Int

main :: IO ()
main = do
  print (describe (Proxy @(Person "Benoit" "Mandelbrot")))

  print (getUserDescription
    @_
    @4321
    (Proxy @MandelbrotUser))



type Hausdorff  = Person "Felix" "Hausdorff"
type Mandelbrot = Person "Benoit" "Mandelbrot"

data TLTip
data TLList v next

type Persons = TLList Mandelbrot (TLList Hausdorff TLTip)
