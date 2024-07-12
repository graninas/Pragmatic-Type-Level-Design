{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE GADTs                    #-}

module Main where

import CPrelude


class Eval tag payload ret
  | tag payload -> ret where
  eval :: tag -> Proxy payload -> ret



data IBomb where
  BombWrapper :: a -> IBomb

type family MkBomb a :: IBomb where
  MkBomb a = BombWrapper a



data BombImpl
  (power :: Nat)
    -- ^ Detonation power from 1 to n.
    --   1 == only explodes itself
    --   n == triggers neighbor bombs to explode in the nth radius
type Bomb p = MkBomb (BombImpl p)

data TimerBombImpl
  (turns :: Nat)
    -- ^ How much turns before the bomb explodes
type TimerBomb t = MkBomb (TimerBombImpl t)



data Dig = Dig
data PutFlag = PutFlag
data UseLandmineDetector = UseLandmineDetector



main :: IO ()
main = pure ()
