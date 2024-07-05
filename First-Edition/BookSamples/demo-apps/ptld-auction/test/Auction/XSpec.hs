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
  evX _ _ = pure ()

instance
  EvX AsActImpl NotAnAct (IO ()) where
  evX _ _ = pure ()

instance
  ( mkAct ~ MkAct act
  , EvX AsActImpl act (IO ())
  , EvX AsActImpl acts (IO ())
  ) =>
  EvX AsActImpl (mkAct ': acts) (IO ()) where
  evX _ _ = do
    evX AsActImpl $ Proxy @act
    evX AsActImpl $ Proxy @acts

instance
  EvX AsActImpl '[] (IO ()) where
  evX _ _ = pure ()

instance
  (  EvX AsActImpl acts (IO ())
  ) =>
  EvX AsActImpl (ScriptHolder acts) (IO ()) where
  evX _ _ = evX AsActImpl $ Proxy @acts

evScript :: IO ()
evScript = do

  evX AsActImpl $ Proxy @MyHolder


spec :: Spec
spec = do
  describe "x" $ do

    xit "x" $ do


      1 `shouldBe` 2
