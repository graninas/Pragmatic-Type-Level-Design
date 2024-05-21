{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Proxy ( Proxy(..) )

-- Existential Fight Club in Haskell

-- Demonstration of the existential wrapping approach.


-- Type class interface
class FightClubRule rule where
  explain :: Proxy rule -> String


-- Rules (can be just empty ADTs)
data FirstRule
data SecondRule
data ThirdRule

instance FightClubRule FirstRule where
  explain _ = "You do not talk about Fight Club."

instance FightClubRule SecondRule where
  explain _ = "You DO NOT talk about Fight Club."

instance FightClubRule ThirdRule where
  explain _ = "If someone says stop, goes limp, or taps out, the fight is over."


-- Existential wrapper
data Secrecy where
  Secrecy :: FightClubRule rule => Proxy rule -> Secrecy

-- Forming a list of opaque existential values
rules :: [Secrecy]
rules = [ Secrecy (Proxy @FirstRule)
        , Secrecy (Proxy @SecondRule)
        , Secrecy (Proxy @ThirdRule)
        ]


main :: IO ()
main = mapM_ (\(Secrecy proxy) -> putStrLn $ explain proxy) rules
