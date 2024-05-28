{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Main where

import Data.Proxy ( Proxy(..) )


-- Implement Rust's OOP, not Java's

-- Can't be an ADT because of the ListTypeTag.
-- It's not clear how to uniformly provide arbitrary types
-- without a type variable.
-- data TypeTag
--   = StringTypeTag
--   | SpecialTypeTag Symbol
--   | ByteArrayTypeTag
--   | DateTypeTag
--   | ListTypeTag  ?????



data StringTypeTag
data IntefaceTypeTag iface
data ByteArrayTypeTag
data DateTypeTag
data ListTypeTag objType



data PropertyType lvl
  = Property
    { ptName :: StringType lvl
    , ptType :: TypeTag
    }


data StructType lvl
  = Struct
    { stProperties :: [PropertyType lvl]
    }


type IMime = Inteface
  '[ Method "Prepare" '[ Argument EMailData ]
  ]



data AttachmentTag

type AttachmentData = Struct AttachmentTag
  '[ Property "name" StringTypeTag
  --  , Property "mime" (SpecialTypeTag "mime")
  --  , Property "mime"  (IntefaceTypeTag IMime)
   , Property "blob" ByteArrayTypeTag
   ]

type AttachmentImpl = Impl AttachmentTag
  '[

   ]

type EMailData = Struct
  '[ Property "source" StringTypeTag
   , Property "date" DateTypeTag
   , Property "subject" StringTypeTag
   , Property "text" StringTypeTag
   , Property "attachments" (ListTypeTag AttachmentData)
   ]


type IEmail = Inteface
  '[ Method "Prepare" '[ Argument EMailData ]
   , Method "AddAttachment"
      '[ Argument AttachmentData
       ]
   , Method "Send" '[]
   ]




-- -- | Abstract door. Should not be directely materialized.
-- type AbstractDoor = AbstractProp (Group EAbstractDoor)
--   '[ PropKeyVal EIcon (OwnProp (IconVal "+"))   -- TODO: open and close door with own icons
--    , PropKeyVal EHP   (OwnProp (HPVal 50))
--    , PropKeyVal EPos  (OwnProp DerivedPosVal)

--     -- | Possible states
--    , PropKeyBag EStates
--       '[ OwnProp (StaticPropRef StateOpen)
--        , OwnProp (StaticPropRef StateClose)
--        ]

--     -- | Current state. Points to a close/open state
--    , PropKeyVal EState (OwnProp StatePropRefVal)

--     -- | Abilities to react to effects
--    , PropKeyBag EAbilities
--       '[ SharedProp (PropScript (Group EPushable)
--                     PushableScript)
--        ]
--    ]


-- -- | Specific door with a specific icon.
-- type SpecificDoor = DerivedProp ESpecificDoor AbstractDoor
--   '[ PropKeyVal EIcon (OwnProp (IconVal "?"))   -- TODO: open and close door with own icons
--    , PropKeyVal EHP   (OwnProp (HPVal 100))
--    , PropKeyVal EPos  (OwnProp (PosVal 2 3))
--    ]

main :: IO ()
main = pure ()
