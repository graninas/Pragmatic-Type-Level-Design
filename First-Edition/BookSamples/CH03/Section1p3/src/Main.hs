{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.Proxy
import GHC.TypeLits

import Control.Monad.Reader


class Description (a :: any) where
  describe :: Proxy a -> String

instance Description Bool where
  describe _ = "???"

instance Description 'True where
  describe _ = "verified"

instance Description 'False where
  describe _ = "not verified"

class BoolDescription (a :: Bool) where
  describeBool
    :: Proxy (a :: Bool)
    -> String     -- on True
    -> String     -- on False
    -> String

instance BoolDescription 'True where
  describeBool _ onTrue _ = onTrue

instance BoolDescription 'False where
  describeBool _ _ onFalse = onFalse

data Person (firstName :: Symbol) (lastName :: Symbol)
data User
  (login :: Symbol)
  (verified :: Bool)
  person


type MandelbrotPerson = Person "Benoit" "Mandelbrot"
type MandelbrotUser = User "mandel" 'True MandelbrotPerson

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
