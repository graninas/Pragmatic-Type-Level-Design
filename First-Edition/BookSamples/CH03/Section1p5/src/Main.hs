{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import GHC.TypeLits

import Control.Monad.Reader


data List a = Empty | Cons a (List a)


class Description (a :: any) where
  describe :: Proxy a -> String

instance Description 'Empty where
  describe _ = "[]"

instance
  ( Description a
  , Description rest
  ) =>
  Description ('Cons a rest) where
  describe _ = describe (Proxy @a)
    <> " : " <> describe (Proxy @rest)


instance Description '[] where
  describe _ = "[]"

instance
  ( Description a
  , Description rest
  ) =>
  Description (a ': rest) where        -- TypeOperators used here
  describe _ = describe (Proxy @a)
    <> " : " <> describe (Proxy @rest)




data PersonType = Person
  { firstName :: Symbol
  , lastName  :: Symbol
  }

data UserType = User
  { login    :: Symbol
  , verified :: Bool
  , person   :: PersonType
  }

type HausdorffPerson = 'Person "Felix" "Hausdorff"
type HausdorffUser   = 'User "haus" 'True HausdorffPerson

type MandelbrotPerson = 'Person "Benoit" "Mandelbrot"
type MandelbrotUser   = 'User "mandel" 'True MandelbrotPerson

type ManualPersonList =
  Cons HausdorffPerson (Cons MandelbrotPerson Empty)

type StockPersonList1 = '[ HausdorffPerson, MandelbrotPerson ]
type StockPersonList2 =
  HausdorffPerson
  ': MandelbrotPerson    -- TypeOperators used here
  ': '[]                 -- TypeOperators used here


main :: IO ()
main = do
  print (describe (Proxy @ManualPersonList))
  print (describe (Proxy @StockPersonList1))
  print (describe (Proxy @StockPersonList2))


instance Description 'True where
  describe _ = "verified"

instance Description 'False where
  describe _ = "not verified"

instance
  (KnownSymbol fn, KnownSymbol ln) =>
  Description ('Person fn ln) where
  describe _ =
    symbolVal (Proxy @fn) <> " " <> symbolVal (Proxy @ln)

instance
  ( KnownSymbol login
  , Description person
  , Description verified
  ) =>
  Description ('User login verified person) where
  describe _ = symbolVal (Proxy @login)
    <> " " <> describe (Proxy @person)
    <> " (" <> describe (Proxy @verified)
    <> ")"

getUserDescription
  :: forall login pincode verified person
   . ( KnownSymbol login
     , KnownNat pincode
     , Description person
     , Description verified
     )
  => Proxy ('User login verified person)
  -> String
getUserDescription _
     = "User: " <> symbolVal (Proxy @login)
    <> "verified: " <>  describe (Proxy @verified)
    <> ", pincode: " <> show (natVal (Proxy @pincode))
    <> ", Person: " <> describe (Proxy @person)
