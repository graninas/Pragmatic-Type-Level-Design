{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

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


data IAct

type family MkAction a :: IAct

data ReadXImpl (x :: Symbol)
data WriteXImpl (x :: Symbol)
-- type ReadX x = MkHList (ReadXImpl x)
-- type WriteX x = MkHList (ReadXImpl x)
type ReadX x  = ReadXImpl x
type WriteX x = ReadXImpl x

data NotAnAct

type Script' =
  '[ ReadX "abc"
   , ReadX "cde"
   , WriteX "efg"
   , NotAnAct
  --  , Script
   ]

-- type Script =
--   ReadX "abc"
--   :> ReadX "cde"
--   :> WriteX "efg"
--   :> HEmptyImpl

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
  ( EvX AsActImpl act (IO ())
  , EvX AsActImpl acts (IO ())
  ) =>
  EvX AsActImpl (act ': acts) (IO ()) where
  evX _ _ = pure ()

instance
  EvX AsActImpl '[] (IO ()) where
  evX _ _ = pure ()

-- instance
--   EvX
--     AsActImpl
--     ( (ReadXImpl "abc")
--      :> ( (ReadXImpl "cde")
--          :> ( (ReadXImpl "efg")
--              :>  HEmptyImpl)))
--     (IO ()) where
--   evX _ _ = pure ()



evScript :: IO ()
evScript = do

  -- evX AsActImpl $ Proxy @Script
  evX AsActImpl $ Proxy @Script'


spec :: Spec
spec = do
  describe "x" $ do

    xit "x" $ do


      1 `shouldBe` 2
