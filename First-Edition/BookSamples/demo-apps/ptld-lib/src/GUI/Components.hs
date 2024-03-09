-- imaginary-gui library for GUIs.
-- Not an actual implementation, just a sample of a high-level interface.

{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies      #-}

module GUI.Components where

import Prelude

import GHC.TypeNats
import GHC.TypeLits
import Data.Proxy


-- N.B. Not a real library, just a demo interface.

-- | It’s a TextBox.
data TextBox (n :: Nat) (str :: Symbol)
  deriving Show

-- | It's a RowLayout.
data RowLayout (items :: [*])

-- | It's a ColumnLayout.
data ColumnLayout (items :: [*])

-- | It's a TextField.
data TextField (cid :: Symbol) (cap :: Symbol)

-- | It's a TextField.
data TextFieldWithMode (cid :: Symbol) (cap :: Symbol) (m :: *)

-- | TextField password mode.
data PasswordEchoOnEdit
