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

module Tests.InterfacesSpec where

import CPrelude

import qualified Prelude

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Dynamic as Dyn
import GHC.TypeLits


data IAction where
  ActionWrapper :: a -> IAction

type family MkAction a :: IAction where
  MkAction a = ActionWrapper a

data ReadXImpl (x :: Symbol)
data WriteXImpl (x :: Symbol)
data NotAnAction
type ReadX x  = MkAction (ReadXImpl x)
type WriteX x = MkAction (ReadXImpl x)

class Eval tag payload ret
  | tag payload -> ret where
  eval :: tag -> Proxy payload -> ret

data AsActImpl = AsActImpl

instance
  ( KnownSymbol n
  ) =>
  Eval AsActImpl (ReadXImpl n) (IO ()) where
  eval _ _ = do
    print "f"
    print $ symbolVal $ Proxy @n
    pure ()

instance
  ( Eval AsActImpl act (IO ())
  , Eval AsActImpl acts (IO ())
  ) =>
  Eval AsActImpl (ActionWrapper act ': acts) (IO ()) where
  eval _ _ = do
    print "d"
    eval AsActImpl $ Proxy @act
    eval AsActImpl $ Proxy @acts

instance
  Eval AsActImpl '[] (IO ()) where
  eval _ _ = do
    print "c"
    pure ()

instance
  ( Eval AsActImpl acts (IO ())
  ) =>
  Eval AsActImpl (ScriptHolder acts) (IO ()) where
  eval _ _ = do
    print "b"
    eval AsActImpl $ Proxy @acts



data ScriptHolder (acts :: [IAction])

type Script =
  '[ ReadX "abc"
   , ReadX "cde"
   , WriteX "efg"
  --  , NotAnAction     -- Won't compile
   ]
type MyHolder = ScriptHolder Script

evalScript :: IO ()
evalScript = do
  print "a"
  eval AsActImpl $ Proxy @MyHolder


spec :: Spec
spec = do
  describe "x" $ do

    xit "x" $ do

      evalScript
      1 `shouldBe` 2
