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

instance
  (KnownSymbol login, Description person) =>
  Description (User login person) where
  describe _ = symbolVal (Proxy @login)
    <> " " <> describe (Proxy @person)


getUserDescription
  :: forall login pincode person fn ln
   . ( KnownSymbol login
     , KnownNat pincode
     , KnownSymbol fn
     , KnownSymbol ln
     )
  => Proxy (User login (Person fn ln))
  -> String
getUserDescription _
     = "User: " <> symbolVal (Proxy @login)
    <> ", pincode: " <> show (natVal (Proxy @pincode))
    <> ", person: " <> symbolVal (Proxy @fn)
    <> " " <> symbolVal (Proxy @ln)

type MandelbrotPerson = Person "Benoit" "Mandelbrot"
type MandelbrotUser = User "mandel" MandelbrotPerson
type InvalidUser = User "invalid" Int

main :: IO ()
main = do
  print (describe (Proxy @(Person "Benoit" "Mandelbrot")))

  print (getUserDescription
    @_
    @4321
    (Proxy @MandelbrotUser))

  print (describe (Proxy @MandelbrotUser))
