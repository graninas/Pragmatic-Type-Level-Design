{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | Static property model pretty printers.
module TypeLevel.ZeplrogOOP.Static.Description where

import CPrelude
import qualified Prelude as P (unwords)
import qualified Data.Map as Map

import TypeLevel.ZeplrogOOP.Static.Model


type Indent = Int
type Raw = Bool
type Printer = State (Indent, Raw, [String]) ()

class SPrint item where
  sPrint :: item -> Printer

describe :: SPrint item => item -> [String]
describe item = let
  (_, _, ss) = execState (sPrint item) (0, False, [])
  in reverse ss

push :: String -> Printer
push line = do
  (i, r, ss) <- get
  let line' = replicate (i * 2) ' ' <> line
  put (i, r, line' : ss)

add
  :: SPrint item
  => item -> Printer
add item = do
  let (_, _, rawSS) = execState (sPrint item) (0, True, [])
  let rawS = ' ' : P.unwords rawSS
  (i, r, ss) <- get
  case ss of
    [] -> put (i, r, [rawS])
    (s' : ss') -> put (i, r, (s' <> rawS) : ss')

addS :: String -> Printer
addS s = do
  (i, r, ss) <- get
  case ss of
    [] -> put (i, r, [s])
    (s' : ss') -> put (i, r, (s' <> s) : ss')

indent :: Printer
indent = do
  (i, r, ss) <- get
  put (i + 1, r, ss)

deIndent :: Printer
deIndent = do
  (i, r, ss) <- get
  put (i - 1, r, ss)

sub
  :: SPrint item
  => item -> Printer
sub item = do
  (_, r, _) <- get
  unless r indent
  sPrint item
  unless r deIndent

instance SPrint TagPropertyGroupVL where
  sPrint (TagGroup ess) = do
    push "TagGroup "
    add ess

  sPrint (TagGroupRoot ess tagProp) = do
    push "TagGroupRoot "
    add ess
    sub tagProp

instance SPrint TagPropertyVL where
  sPrint (TagProp singGroup) = do
    push "TagProp "
    sub singGroup

instance SPrint PropertyVL where
  sPrint (TagPropRef sProp) = do
    push "TagPropRef"
    sub sProp

  sPrint (PropDict group kvs scripts) = do
    push "PropDict "
    sub group
    sub kvs

instance SPrint PropertyGroupVL where
  sPrint (GroupId ess statPropId) = do
    push "GroupId "
    add ess
    add statPropId

  sPrint (GroupRootId ess statPropId prop) = do
    push "GroupRootId"
    add ess
    add statPropId
    sub prop

  -- sPrint (GroupRoot _ _) = do
  --   push "Err: static group root can't be printed"

  -- sPrint (Group _) = do
  --   push "Err: static group can't be printed"

instance SPrint EssenceVL where
  sPrint (Ess ess) = do
    push ("<" <> ess <> ">")

instance SPrint [PropertyKeyValueVL] where
  sPrint kvs = mapM_ sPrint kvs

instance SPrint StaticPropertyId where
  sPrint (StaticPropertyId i) = do
    push ("(SPID: " <> show i)
    addS ")"

instance SPrint PropertyKeyValueVL where
  sPrint (PropKeyBag ess owns) = do
    push "PropKeyBag"
    add ess
    mapM_ sub owns

  sPrint (PropKeyVal ess own) = do
    push "PropKeyVal"
    add ess
    sub own

instance SPrint PropertyOwningVL where
  sPrint (OwnVal valDef) = do
    push "OwnVal"
    add valDef

  sPrint (OwnProp prop) = do
    push "OwnProp"
    sub prop

  sPrint (SharedProp prop) = do
    push "SharedProp"
    sub prop

instance SPrint ValDefVL where
  sPrint (IntValue i) = do
    push ("(IntValue: " <> show i)
    addS ")"

  sPrint (BoolValue f) = do
    push ("(BoolValue: " <> show f)
    addS ")"

  sPrint (PairValue p1 p2) = do
    push "(PairValue: ("
    add p1
    add p2
    addS ")"
    addS ")"

  sPrint (StringValue s) = do
    push ("(StringValue: " <> s)
    addS ")"

  sPrint (PathValue esss) = do
    push "(PathValue: "
    mapM_ add esss
    addS ")"
