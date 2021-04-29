{-# OPTIONS -fno-warn-orphans #-}

module CPrelude
  ( module X
  ) where

import           Control.Concurrent           as X (ThreadId, forkIO,
                                                    killThread, threadDelay)
import           Control.Concurrent.STM       as X (retry)
import           Control.Concurrent.STM.TMVar as X (TMVar, newEmptyTMVar,
                                                    newEmptyTMVarIO, newTMVar,
                                                    newTMVarIO, putTMVar,
                                                    readTMVar, takeTMVar,
                                                    tryReadTMVar)
import           Control.Concurrent.STM.TVar  as X (modifyTVar)
import           Control.Exception            as X (SomeException (..))
import           Control.Monad                as X (liftM, unless, void, when)
import           Control.Newtype.Generics     as X (Newtype, O, pack, unpack)
import           Data.Maybe                   as X (fromJust, fromMaybe)
import           GHC.Base                     as X (until)
import           GHC.Generics                 as X (Generic)
import           Text.Read                    as X (read, readsPrec)

-- includes Data.IORef
import           Universum                    as X hiding (All, Option, Set,
                                                    Type, head, init, last, set,
                                                    tail, trace)
import           Universum.Functor.Fmap       as X ((<<$>>))
import           Universum.Unsafe             as X (head, init, last, tail,
                                                    (!!))

import Debug.Trace as X (trace)
