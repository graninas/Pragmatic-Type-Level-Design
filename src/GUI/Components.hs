-- imaginary-gui library for GUIs.
-- Not an actual implementation, just a sample of a high-level interface.

{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies      #-}

module GUI.Components where

import GHC.TypeNats
import GHC.TypeLits
import Data.Proxy


-- | TextBox
data TextBox (n :: Nat) (str :: Symbol)
  deriving Show

-- | ColumnLayout
data ColumnLayout (its :: [*])

-- | TextField
data TextField (cid :: Symbol) (cap :: Symbol)

-- | TextField
data TextFieldWithMode (cid :: Symbol) (cap :: Symbol) (m :: *)

-- TextField password mode
data PasswordEchoOnEdit
