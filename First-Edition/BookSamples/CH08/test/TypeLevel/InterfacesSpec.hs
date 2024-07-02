{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module TypeLevel.InterfacesSpec where

import CPrelude

import TypeLevel.Interfaces

import Test.Hspec

-- type AbstractDoor = AbstractDerivedProp EAbstractDoor AnyProp
--   '[ PropKeyVal EIcon (OwnVal (IconVal "+"))
--    , PropKeyVal EHP   (OwnVal (HPTagVal 50))
--    , PropKeyVal EPos  (OwnVal (DerivablePosTagVal 0 0))

--     -- | Possible states
--    , PropKeyBag EStates
--       '[ TagPropRef StateOpen
--        , TagPropRef StateClose
--        ]

--     -- | Current state. Points to a close/open state
--    , PropKeyVal EState (OwnVal (PathValue CloseStateRef))
--    ]
--    '[PropScript EOpenDoorScript OpenDoorScript]


-- -- | Specific door with a specific icon.
-- type SpecificDoor = DerivedProp ESpecificDoor AbstractDoor
--   '[ PropKeyVal EIcon (OwnVal (IconVal "?"))
--    , PropKeyVal EHP   (OwnVal (HPTagVal 50))
--    , PropKeyVal EPos  (OwnVal (PosTagVal 2 3))    -- overridden
--    ]
--    '[]

type EAbstractDoor   = Essence "object:abstract door"
type ESpecificDoor   = Essence "object:specific door"

type AbstractDoor = AbstractProperty (Group EAbstractDoor)
  ( Fields
      '[
       ]
  )

type SpecificDoor = Property ESpecificDoor AbstractDoor
  ( Fields
      '[

       ]
  )



-- type UKOnly  = Censorship (ExtL.AllowedCountries "UK only" '[UK])
-- type UKAndUS = Censorship (ExtL.AllowedCountries "UK & US" '[UK, US])

-- type MinBid202 = 'ValNameS "202 min bid"
-- type PayloadLot1 = LotPayload (ExtL.EFLotPayload (MoneyVal "1000"))
-- type PayloadLot2 = LotPayload (ExtL.EFLotPayload (MoneyDynVal MinBid202))
-- type PayloadLot3 = LotPayload (ExtL.EFLotPayload (MoneyVal "40000"))

-- -- Auction algorithm

-- type EnglishAuctionLotAction1 =
--   ( Action (GetLotName (ConcatL "New lot: " Print))
--   ( Action (GetLotDescr Print)
--     End
--   ))

-- type EnglishAuctionLotAction2 =
--   ( Action (GetLotName2  (ConcatL "New lot: "         (Both (WriteRef "LotName result" String)  Print)))
--   ( Action (GetLotDescr2 (ConcatL "Lot description: " (Both (WriteRef "LotDescr result" String) Print)))
--     End
--   ))

-- type EnglishAuctionFlow = AuctionFlow
--   ( LotProcess EnglishAuctionLotAction1
--   )


-- type TestFlow = AuctionFlow
--   ( LotProcess
--       ( Action (ReadRef "curRound" Int Print)
--         ( Action (ReadRef "curCost" Int Drop)
--           End
--         )
--       )
--   )

-- -- Auction

-- type WorldArtsInfo = Info "World arts" "UK Bank"
-- type WorldArtsLots = Lots
--   '[ Lot "101" "Dali artwork"      PayloadLot1 (Currency ExtL.GBP) UKOnly
--    , Lot "202" "Chinese vase"      PayloadLot2 (Currency ExtL.USD) UKAndUS
--    , Lot "303" "Ancient mechanism" PayloadLot3 (Currency ExtL.USD) NoCensorship
--    ]

-- type WorldArtsAuction = Auction
--   EnglishAuctionFlow
--   WorldArtsInfo
--   WorldArtsLots

-- type TestAuction = Auction
--   TestFlow
--   WorldArtsInfo
--   WorldArtsLots

spec :: Spec
spec = do
  describe "Type level interfaces" $ do

    xit "Property materialization test" $ do
      1 `shouldBe` 2
