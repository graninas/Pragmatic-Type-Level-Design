{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE GADTs         #-}

module CH01.S01_05_2 where

import Data.Text


data Widget
  = ColumnLayout [Widget]
  | ValidatedTextField TextValidator
  | ValidatedIntField IntValidator
  | ValidatedCanvas CanvasValidator

data TextValidator = AlphaNumeric | DigitsOnly
data IntValidator = PositiveNumber | NegativeNumber
data CanvasValidator = NotBlank

loginInput :: Widget
loginInput = ColumnLayout
  [ ValidatedTextField AlphaNumeric
  , ValidatedIntField PositiveNumber
  , ValidatedCanvas NotBlank
  ]
