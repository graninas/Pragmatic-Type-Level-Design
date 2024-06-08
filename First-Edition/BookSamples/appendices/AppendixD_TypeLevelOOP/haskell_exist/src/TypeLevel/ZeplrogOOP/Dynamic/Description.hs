{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | Dynamic property model pretty printers.
module TypeLevel.ZeplrogOOP.Dynamic.Description where

import CPrelude
import qualified Prelude as P (unwords)
import qualified Data.Map as Map

import TypeLevel.ZeplrogOOP.Dynamic.Model
import qualified TypeLevel.ZeplrogOOP.Static.Model as SMod
import qualified TypeLevel.ZeplrogOOP.Static.Description as SPrint
import qualified TypeLevel.ZeplrogOOP.Static.Query as SQ


type Indent = Int
type Raw = Bool
type Printer = StateT (Indent, Raw, [String]) IO ()

class DPrint item where
  dPrint :: item -> Printer

describe :: DPrint item => item -> IO [String]
describe item = do
  (_, _, ss) <- execStateT (dPrint item) (0, False, [])
  pure $ reverse ss

push :: String -> Printer
push line = do
  (i, r, ss) <- get
  let line' = replicate (i * 2) ' ' <> line
  put (i, r, line' : ss)

add
  :: DPrint item
  => item -> Printer
add item = do
  (_, _, rawSS) <- liftIO $ execStateT (dPrint item) (0, True, [])
  let rawS = P.unwords rawSS
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
  :: DPrint item
  => item -> Printer
sub item = do
  (_, r, _) <- get
  unless r indent
  dPrint item
  unless r deIndent

instance DPrint Property where
  dPrint (TagPropRef sProp) = do
    push "TagPropRef"
    let descr = SPrint.describe sProp
    indent
    mapM_ push descr
    deIndent

  dPrint (Prop pId mbOwner sPid fieldsRef scripts) = do
    push "Prop"
    add pId
    add sPid
    case mbOwner of
      Just owner -> do
        addS "(Owner:"
        add owner
        addS ")"
      _ -> pure ()

    fields <- liftIO $ readIORef fieldsRef
    mapM_ sub $ Map.toList fields

instance DPrint (Essence, PropertyOwning) where
  dPrint (ess, own) = do
    push "Field ["
    addS ess
    addS "] "
    sub own

instance DPrint PropertyId where
  dPrint (PropertyId pId) = do
    addS $ "(PID: " <> show pId <> ")"

instance DPrint SMod.StaticPropertyId where
  dPrint (SMod.StaticPropertyId spid) = do
    addS $ "<SPID: " <> show spid <> ">"

instance DPrint PropertyOwning where
  dPrint (OwnVal ref) = do
    val <- liftIO $ readIORef ref
    push "Val:"
    add val

  dPrint (OwnProp prop) = do
    sub prop

  dPrint (SharedProp propRef) = do
    -- sub prop
    pure ()

  dPrint (OwnDict var) = do
    -- sub prop
    pure ()

instance DPrint Value where
  dPrint (PairValue val1 val2) = do
    addS "("
    add val1
    addS ", "
    add val2
    addS ")"

  dPrint (IntValue i) = do
    addS $ show i

  dPrint (BoolValue b) = do
    addS $ show b

  dPrint (StringValue s) = do
    addS s

  dPrint (TagValue tagProp val) = do
    let SMod.Ess tpEss = SQ.getTagPropEss tagProp
    addS "TagVal"
    dPrint val

    let descr = SPrint.describe tagProp
    indent
    mapM_ push descr
    deIndent

  dPrint (Path esss) = do
    addS "Path:"
    addS $ show $ map (\e -> "<" <> e <> ">") esss

  dPrint (StaticPropertyRefValue spid) = do
    addS "StaticPropertyRefValue:"
    addS $ show spid

