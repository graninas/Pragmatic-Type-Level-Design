module Extra.Implementation where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Game.Types
import Minefield.Game.System
import Minefield.Game.UI

import Minefield.Extensions.Materialization
import Minefield.Extensions.Implementation
import Minefield.Extensions.Nouns.TimerBomb
import Minefield.Extensions.Verbs.PutFlag

import GHC.TypeLits


-- TimerBomb

instance
  ( KnownSymbol i
  , KnownSymbol ot
  , KnownNat turns
  ) =>
  Eval (SystemBus, Pos) MakeActor (TimerBombImpl i ot turns) Actor where
  eval (sysBus, pos) _ _ = do
    tickChan <- createTickChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot
    let turns = fromIntegral $ natVal $ Proxy @turns

    let oInfo = ObjectInfo icon pos oType True []
    obj <- TimerBombObject
      <$> newIORef oInfo
      <*> newIORef turns

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processTimerBombEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isObjectRequestEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar

processTimerBombEvent
  :: SystemBus
  -> TimerBombObject
  -> SystemEvent
  -> GameIO ()
processTimerBombEvent sysBus obj commonEv =
  processCommonEvent sysBus (tboObjectInfoRef obj) commonEv
