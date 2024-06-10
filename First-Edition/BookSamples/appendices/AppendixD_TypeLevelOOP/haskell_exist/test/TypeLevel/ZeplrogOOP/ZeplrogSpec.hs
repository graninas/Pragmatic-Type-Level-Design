{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeLevel.ZeplrogOOP.ZeplrogSpec where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model
import TypeLevel.ZeplrogOOP.Static.Query
import TypeLevel.ZeplrogOOP.Static.Materialization
import qualified TypeLevel.ZeplrogOOP.Dynamic.Model as DMod
import qualified TypeLevel.ZeplrogOOP.Dynamic.Instantiation as DInst
import qualified TypeLevel.ZeplrogOOP.Dynamic.Interaction as Interact
import qualified TypeLevel.ZeplrogOOP.Dynamic.Query as Q
import qualified TypeLevel.ZeplrogOOP.Static.Description as SPrint
import qualified TypeLevel.ZeplrogOOP.Dynamic.Description as DPrint

import TypeLevel.ZeplrogOOP.Testing.Utils
import TypeLevel.System.Debug

import GHC.TypeLits
import TypeSelector.Granular

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import qualified Data.Map as Map
import qualified Prelude as P (unlines)



-- type EObserving     = Ess @TypeLevel "action:observing"
-- type EDiscovering   = Ess @TypeLevel "action:discovering"
-- type ESettingGoals  = Ess @TypeLevel "action:setting goals"
-- type EPlanning      = Ess @TypeLevel "action:planning"
-- type EFollowingPlan = Ess @TypeLevel "action:following plan"
-- type EIdling        = Ess @TypeLevel "action:idling"
-- type ENoAction      = Ess @TypeLevel "action:no action"

-- type EActionLoop    = Ess @TypeLevel "system:action loop"
-- type EGoal          = Ess @TypeLevel "system:goal"

-- type EInventory     = Ess @TypeLevel "category:inventory"
-- type EIntrinsics    = Ess @TypeLevel "category:intrinsics"
-- type EAbilities     = Ess @TypeLevel "category:abilities"
type EAnyProp = Ess @TypeLevel "category:any prop"

type EStates        = Ess @TypeLevel "category:states"

-- type EStateRef      = Ess @TypeLevel "ref:state"
type EState         = Ess @TypeLevel "state"
type EOpen          = Ess @TypeLevel "open"
type EClose         = Ess @TypeLevel "close"
type EStateOpen     = Ess @TypeLevel "state:open"
type EStateClose    = Ess @TypeLevel "state:close"
-- type EPushable      = Ess @TypeLevel "ability:pushable"

-- type EPushableScript = Ess @TypeLevel "script:pushable"

-- type EEmptySpace    = Ess @TypeLevel "object:empty space"
-- type EWall          = Ess @TypeLevel "object:wall"
-- type EWand          = Ess @TypeLevel "object:wand"
-- type ERat           = Ess @TypeLevel "object:rat"
-- type EGuard         = Ess @TypeLevel "object:guard"
type EAbstractDoor   = Ess @TypeLevel "object:abstract door"
type ESpecificDoor   = Ess @TypeLevel "object:specific door"
type EDoor           = Ess @TypeLevel "object:door"
type EOpenDoorScript = Ess @TypeLevel "script:open door"

-- type EPhysicalImpact = Ess @TypeLevel "effect:physical impact"
-- type EPush           = Ess @TypeLevel "effect:push"
-- type EShockwave      = Ess @TypeLevel "effect:shockwave"
-- type EKick           = Ess @TypeLevel "effect:kick"

type EIcon = Ess @TypeLevel "system:icon"
type EGenericPos = Ess @TypeLevel "intrinsics:generic pos"
type EPos = Ess @TypeLevel "intrinsics:pos"
type EGenericHP = Ess @TypeLevel "intrinsics:generic hp"
type EHP = Ess @TypeLevel "intrinsics:hp"
-- type EStrength      = Ess @TypeLevel "intrinsics:strength"




-- | General root property for everything that is open or close.
type Open  = TagProp (TagGroup EOpen)
type Close = TagProp (TagGroup EClose)

type StateOpen  = TagProp (TagGroupRoot EStateOpen Open)
type StateClose = TagProp (TagGroupRoot EStateClose Close)



-- type PushableScript = SimpleScript EPushableScript
--   '[ SimpleQuery
--         '[ FollowReferences ]
--         '[ QEssence EState, QGetEssence ]
--          (BoolVar "is open")
--    ]
--   '[ ConditionalAction
--       (ConditionDef "is open" QEq (BoolValue True))
--       (ReplaceProp '[ EState ] '[ EStates, EStateClose ])
--    , ConditionalAction
--       (ConditionDef "is open" QEq (BoolValue False))
--       (ReplaceProp '[ EState ] '[ EStates, EStateOpen ])
--    ]


-- type PathToIcon = IconPath '[ EIcon ]
-- type PathToPos  = PosPath  '[ EPos ]

-- | World position value.
type GenericPos    = TagProp (TagGroup EGenericPos)
type PosVal x y    = PairValue (IntValue x) (IntValue y)
type PosTagVal x y = TagValue GenericPos (PosVal x y)


-- -- | Derived world position value.
-- type DerivedPosVal = PropVal
--   (GroupRoot EPos GenericPos)
--   DerivedWorldPos

-- | Strength random val
-- type StrengthRandomVal from to = RandomIntValue from to

type IconVal icon = StringValue icon

-- | HP value: current and max
type GenericHP   = TagProp (TagGroup EGenericHP)
type HPVal hp    = PairValue (IntValue hp) (IntValue hp)
type HPTagVal hp = TagValue GenericHP (HPVal hp)

type CloseStateRef = '[ EStates, EStateClose ]
type OpenStateRef  = '[ EStates, EStateOpen  ]

type OpenDoorScript = 'Script @'TypeLevel "opens a door"
  '[ WriteData (ToField 'Proxy '[EState])
               (FromConst (PathConst OpenStateRef))
   ]

type AnyProp = AbstractProp (Group EAnyProp) '[] '[]

-- | Abstract door
type AbstractDoor = AbstractDerivedProp EAbstractDoor AnyProp
  '[ PropKeyVal EIcon (OwnVal (IconVal "+"))
   , PropKeyVal EHP   (OwnVal (HPTagVal 50))
  --  , PropKeyVal EPos  (OwnVal DerivedPosVal)    --------

    -- | Possible states
   , PropKeyBag EStates
      '[ TagPropRef StateOpen
       , TagPropRef StateClose
       ]

    -- | Current state. Points to a close/open state
   , PropKeyVal EState (OwnVal (PathValue CloseStateRef))

    -- | Abilities to react to effects              -------
  --  , PropKeyBag EAbilities
  --     '[ SharedProp (PropScript (Group EPushable)
  --                   PushableScript)
  --      ]
   ]
   '[PropScript EOpenDoorScript OpenDoorScript]

-- | Specific door with a specific icon.
type SpecificDoor = DerivedProp ESpecificDoor AbstractDoor
  '[ PropKeyVal EIcon (OwnVal (IconVal "?"))
   , PropKeyVal EHP   (OwnVal (HPTagVal 50))
   , PropKeyVal EPos  (OwnVal (PosTagVal 2 3))
   ]
   '[]

spec :: Spec
spec = describe "Zeplrog data model" $ do
  it "Door instantiation test" $ do
    (sEnv, dEnv) <- DInst.makeEnvs DebugDisabled

    doorStat <- sMat' sEnv () $ Proxy @SpecificDoor
    door <- DInst.dInstParent dEnv Nothing doorStat

    let iconValPath = [toDynEss @EIcon]

    descr <- DPrint.describe door
    putStrLn $ P.unlines descr

    val1 <- Q.readStringVal door iconValPath
    val1 `shouldBe` "?"

    let openStateRef  = toDynEsss @OpenStateRef
    let closeStateRef = toDynEsss @CloseStateRef

    let stateValPath  = [toDynEss @EState]
    val2 <- Q.readPathVal door stateValPath
    val2 `shouldBe` closeStateRef

    let openDoorScript = toDynEss @EOpenDoorScript
    Interact.invoke openDoorScript door

    val3 <- Q.readPathVal door stateValPath
    val3 `shouldBe` openStateRef
