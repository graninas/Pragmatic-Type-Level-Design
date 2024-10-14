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

type Shakespeare  = 'Person "William" "Shakespeare"
type Byron        = 'Person "George Gordon" "Byron"
type Pushkin      = 'Person "Alexander" "Pushkin"

type PoetsList =
  Cons Shakespeare (Cons Byron ( Cons Pushkin Empty))

type StockList1 = '[ Shakespeare, Byron, Pushkin ]
type StockList2 =
  Shakespeare
  ': Byron               -- TypeOperators used here
  ': Pushkin
  ': '[]


main :: IO ()
main = do
  print (describe (Proxy @PoetsList))
  print (describe (Proxy @StockList1))
  print (describe (Proxy @StockList2))


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
