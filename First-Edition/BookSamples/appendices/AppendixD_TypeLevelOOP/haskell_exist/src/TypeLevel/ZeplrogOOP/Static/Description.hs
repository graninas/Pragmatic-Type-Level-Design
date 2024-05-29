{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | Static property model pretty printers.
module TypeLevel.ZeplrogOOP.Static.Description where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model


describe :: PropertyVL -> [String]
describe prop = let
  (_, ss) = execState (sPrint prop) (0, [])
  in reverse ss

type Indent = Int
type Printer = State (Indent, [String]) ()

class SPrint item where
  sPrint :: item -> Printer


push :: String -> Printer
push line = do
  (i, ss) <- get
  let line' = replicate (i * 2) ' ' <> line
  put (i, line' : ss)

indent :: Printer
indent = do
  (i, ss) <- get
  put (i + 1, ss)

deIndent :: Printer
deIndent = do
  (i, ss) <- get
  put (i - 1, ss)


subSPrint
  :: SPrint item
  => item -> Printer
subSPrint item = do
  indent
  sPrint item
  deIndent

instance SPrint PropertyVL where
  sPrint (StaticProp group) = do
    push "Static property"
    subSPrint group

  sPrint (StaticPropRef prop) = do
    push "Static property ref"
    subSPrint prop

  sPrint (PropDict group kvs) = do
    push "Property dict"
    subSPrint group
    subSPrint kvs

  sPrint (AbstractProp group kvs) = do
    push "Abstract property"
    subSPrint group
    subSPrint kvs

  sPrint (DerivedProp ess prop kvs) = do
    push "Derived property"
    subSPrint ess
    subSPrint prop
    subSPrint kvs

instance SPrint PropertyGroupVL where
  sPrint (GroupId ess statPropId) = do
    push "GroupId"
    pure ()

  sPrint (GroupRootId ess statPropId prop) = do
    push "Group root id"
    pure ()

  -- sPrint (GroupRoot _ _) = do
  --   push "Err: static group root can't be printed"

  -- sPrint (Group _) = do
  --   push "Err: static group can't be printed"

instance SPrint EssenceVL where
  sPrint (Ess ess) = do
    push ("Ess: " <> ess)

instance SPrint [PropertyKeyValueVL] where
  sPrint _ = pure ()
