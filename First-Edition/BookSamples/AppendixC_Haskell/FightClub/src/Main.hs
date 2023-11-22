{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Proxy ( Proxy(..) )

class FightClubRule rule where
  explain :: Proxy rule -> String

data FirstRule = FirstRule
data SecondRule = SecondRule
data ThirdRule = ThirdRule

instance FightClubRule FirstRule where
  explain _ = "You do not talk about Fight Club."

instance FightClubRule SecondRule where
  explain _ = "You DO NOT talk about Fight Club."

instance FightClubRule ThirdRule where
  explain _ = "If someone says stop, goes limp, or taps out, the fight is over."

data Secrecy where
  Secrecy :: FightClubRule rule => Proxy rule -> Secrecy

rules :: [Secrecy]
rules = [ Secrecy (Proxy @FirstRule)
        , Secrecy (Proxy @SecondRule)
        , Secrecy (Proxy @ThirdRule)
        ]


main :: IO ()
main = mapM_ (\(Secrecy proxy) -> putStrLn $ explain proxy) rules
