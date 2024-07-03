{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Dyn where

import Prelude

import qualified Data.Dynamic as Dyn
import qualified Data.Text as T
import Data.Text (Text)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep)

import TypeLevelDSL.Context (Key)


toTypeableKey :: forall key. Typeable key => Key
toTypeableKey = show $ typeRep (Proxy :: Proxy key)

toIntDyn :: Int -> Dyn.Dynamic
toIntDyn = Dyn.toDyn

toTextDyn :: Text -> Dyn.Dynamic
toTextDyn = Dyn.toDyn

toFloatDyn :: Float -> Dyn.Dynamic
toFloatDyn = Dyn.toDyn

type DynKV = (Key, Dyn.Dynamic)

toIntDynKV :: forall key. Typeable key => Int -> (Key, Dyn.Dynamic)
toIntDynKV v = (toTypeableKey @key, toIntDyn v)

toTextDynKV :: forall key. Typeable key => Text -> (Key, Dyn.Dynamic)
toTextDynKV v = (toTypeableKey @key, toTextDyn v)

toFloatDynKV :: forall key. Typeable key =>  Float -> (Key, Dyn.Dynamic)
toFloatDynKV v = (toTypeableKey @key, toFloatDyn v)
