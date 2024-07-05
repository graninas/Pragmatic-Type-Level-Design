{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Auction.XSpec where

import CPrelude

import qualified Prelude

import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Context as Ctx

import Test.Hspec
import Auction.Testing.Environment

import Data.HList.HList
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import GHC.TypeLits


data IAct where
  IActW :: a -> IAct

type family MkAct a :: IAct where
  MkAct a = IActW a

data ReadXImpl (x :: Symbol)
data WriteXImpl (x :: Symbol)
data NotAnAct
type ReadX x  = MkAct (ReadXImpl x)
type WriteX x = MkAct (ReadXImpl x)


data ScriptHolder (acts :: [IAct])

type Script =
  '[ ReadX "abc"
   , ReadX "cde"
   , WriteX "efg"
  --  , NotAnAct
   ]
type MyHolder = ScriptHolder Script

class EvX tag payload ret
  | tag payload -> ret where
  evX :: tag -> Proxy payload -> ret

data AsActImpl = AsActImpl


instance
  ( KnownSymbol n
  ) =>
  EvX AsActImpl (ReadXImpl n) (IO ()) where
  evX _ _ = do
    print "f"
    print $ symbolVal $ Proxy @n
    pure ()

instance
  EvX AsActImpl NotAnAct (IO ()) where
  evX _ _ = do
    print "e"
    pure ()

instance
  ( EvX AsActImpl act (IO ())
  , EvX AsActImpl acts (IO ())
  ) =>
  EvX AsActImpl (IActW act ': acts) (IO ()) where
  evX _ _ = do
    print "d"
    evX AsActImpl $ Proxy @act
    evX AsActImpl $ Proxy @acts

instance
  EvX AsActImpl '[] (IO ()) where
  evX _ _ = do
    print "c"
    pure ()

instance
  (  EvX AsActImpl acts (IO ())
  ) =>
  EvX AsActImpl (ScriptHolder acts) (IO ()) where
  evX _ _ = do
    print "b"
    evX AsActImpl $ Proxy @acts

evScript :: IO ()
evScript = do
  print "a"
  evX AsActImpl $ Proxy @MyHolder


spec :: Spec
spec = do
  describe "x" $ do

    it "x" $ do

      evScript
      1 `shouldBe` 2
