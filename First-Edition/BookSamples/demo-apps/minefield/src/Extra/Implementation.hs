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
  ( KnownSymbol ot
  ) =>
  Eval () MakeActorAction
       (ObjAct (TimerBombImpl i ot p) PutFlagImpl)
       (ObjectType, ActorAction) where
  eval () _ _ = do
    let oType = symbolVal $ Proxy @ot

    let act = \sysBus pos -> do
          publishEvent sysBus
            $ ObjectRequestEvent oType pos
            $ AddOverhaulIcon
            $ OverhaulIcon '🚩' (TurnsCount (-1)) (TicksCount 0)
          publishEvent sysBus
            $ ObjectRequestEvent oType pos
            $ SetEnabled False

    pure (oType, act)

instance
  ( KnownSymbol i
  , KnownSymbol ot
  , KnownNat turns
  ) =>
  Eval (SystemBus, Pos) MakeActor (TimerBombImpl i ot turns) Actor where
  eval (sysBus, pos) _ _ = do
    tickChan <- createStepChannel
    queueVar <- createQueueVar

    let icon  = head $ symbolVal $ Proxy @i
    let oType = symbolVal $ Proxy @ot
    let turns = fromIntegral $ natVal $ Proxy @turns

    let oInfo = ObjectInfo icon pos oType True
          $ map toTimerBombOvhIcon [turns .. 1]
    obj <- TimerBombObject
      <$> newIORef oInfo
      <*> newIORef turns

    tId <- forkIO $ actorWorker tickChan queueVar
                  $ processTimerBombEvent sysBus obj
    let sub ev =
          isPopulateCellDescriptionEvent ev
          || isObjectRequestEvent ev
          || isTickEvent ev
          || isTurnEvent ev

    subscribeRecipient sysBus $ Subscription sub queueVar

    pure $ Actor tId tickChan queueVar

toTimerBombOvhIcon :: Int -> OverhaulIcon
toTimerBombOvhIcon turns =
  OverhaulIcon (head $ show turns) (TurnsCount 1) (TicksCount 0)

processTimerBombEvent
  :: SystemBus
  -> TimerBombObject
  -> SystemEvent
  -> GameIO ()
processTimerBombEvent sysBus obj TurnEvent = do
  turnsLeft <- readIORef $ tboTurnsRef obj
  objInf    <- readIORef $ tboObjectInfoRef obj
  case (oiEnabled objInf, turnsLeft) of
    (True, 0)  -> makeExplosion sysBus obj
    (True, n)  -> do
      writeIORef (tboTurnsRef obj) $ turnsLeft - 1
      writeIORef (tboObjectInfoRef obj)
        $ objInf { oiIcon = head $ show n }
    (False, _) -> pure ()

processTimerBombEvent sysBus obj commonEv =
  processCommonEvent sysBus (tboObjectInfoRef obj) commonEv


makeExplosion sysBus obj = error "explosion not impl"
