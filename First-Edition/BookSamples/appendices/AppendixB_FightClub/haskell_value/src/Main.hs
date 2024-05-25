{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Proxy ( Proxy(..) )

-- Valuefied Fight Club in Haskell

-- Demonstration of the valuefied wrapping approach.


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


-- Valuefied wrapper
data Secrecy where
  Secrecy :: (() -> String) -> Secrecy

-- Forming a list of opaque valuefied rules
makeValuefied :: FightClubRule rule => Proxy rule -> Secrecy
makeValuefied proxy = Secrecy (\_ -> explain proxy)

rules :: [Secrecy]
rules = [ makeValuefied (Proxy @FirstRule)
        , makeValuefied (Proxy @SecondRule)
        , makeValuefied (Proxy @ThirdRule)
        ]

-- Accessing the rule's description
explainRule :: Secrecy -> IO ()
explainRule (Secrecy f) = print (f ())

main :: IO ()
main = mapM_ explainRule rules
