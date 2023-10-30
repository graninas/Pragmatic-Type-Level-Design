{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import Data.Proxy
import GHC.TypeLits

class Description (a :: *) where
  describe :: Proxy (a :: *) -> String

class BoolDescription (a :: Bool) where
  describeBool
    :: Proxy (a :: Bool)
    -> String -> String -> String

instance BoolDescription 'True where
  describeBool _ ifTrue _ = ifTrue

instance BoolDescription 'False where
  describeBool _ _ ifFalse = ifFalse


data Person (firstName :: Symbol) (lastName :: Symbol)
data User
  (login :: Symbol)
  (verified :: Bool)
  person


type MandelbrotPerson = Person "Benoit" "Mandelbrot"
type HausdorffPerson = Person "Felix" "Hausdorff"

type MandelbrotUser = User "mandel" 'True MandelbrotPerson
type HausdorffUser = User "haus" 'False HausdorffPerson

main :: IO ()
main = do
  print (describe (Proxy @MandelbrotPerson))

  print (getUserDescription
    @_
    @4321
    (Proxy @MandelbrotUser))





instance
  (KnownSymbol fn, KnownSymbol ln) =>
  Description (Person fn ln) where
  describe _ =
    symbolVal (Proxy @fn) <> " " <> symbolVal (Proxy @ln)

instance
  ( KnownSymbol login
  , Description person
  , BoolDescription verified
  ) =>
  Description (User login verified person) where
  describe _ = symbolVal (Proxy @login)
    <> " " <> describe (Proxy @person)
    <> " (" <> describeBool (Proxy @verified) "verified" "not verified"
    <> ")"

getUserDescription
  :: forall login pincode verified person
   . ( KnownSymbol login
     , KnownNat pincode
     , Description person
     , BoolDescription verified
     )
  => Proxy (User login verified person)
  -> String
getUserDescription _
     = "User: " <> symbolVal (Proxy @login)
    <> "verified: " <>  describeBool (Proxy @verified) "verified" "not verified"
    <> ", pincode: " <> show (natVal (Proxy @pincode))
    <> ", Person: " <> describe (Proxy @person)
