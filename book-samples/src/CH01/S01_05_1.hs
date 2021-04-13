{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE GADTs         #-}

module CH01.S01_05_1 where

import Data.Text


type FieldName = Text
type Caption = Text

data Canvas

data Widget
  = ColumnLayout [Widget]
  | forall t. Validated (Input t) (Validator t)

data Input t = Input FieldName Caption

data Validator t where
  AlphaNumeric   :: Validator Text
  PositiveNumber :: Validator Int
  NotBlank       :: Validator Canvas

loginInput :: Widget
loginInput = ColumnLayout
  [ Validated (Input @Text "login" "Your name:") AlphaNumeric
  , Validated (Input @Int "age" "Your age:") PositiveNumber
  , Validated (Input @Canvas "avatar" "Paint a face:") NotBlank

  -- Won't compile:
  -- , Validated (Input @Int "avatar" "Paint a face:") NotBlank

  , Validated (Input "" "" :: Input Text) AlphaNumeric
  ]
