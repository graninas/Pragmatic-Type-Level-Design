{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Turing.Machine.Language.Materialization where

import Turing.Machine.Language.Rule

import Lib.TypeSelector
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )


-- | Materialization type class.
class Materialize payload (a :: t 'TypeLevel) b
  | payload a -> b where
  mat :: payload -> Proxy a -> b

-- | Helper materialization of lists
class MaterializeList payload l b
  | payload l -> b where
    matList :: payload -> Proxy l -> b


-- Rule materializer

data ConditionsTag cs
data StatesTag ss


-- Rule materializer

instance
  ( KnownSymbol name
  , KnownNat initStateIdx
  , MaterializeList () (StatesTag states) [CustomStateVL]
  ) =>
  Materialize () ('Rule name initStateIdx states) CustomRuleVL where
  mat _ _ = let
    states = matList () $ Proxy @(StatesTag states)
    name = symbolVal $ Proxy @name
    initStateIdx = fromIntegral $ natVal $ Proxy @initStateIdx
    in Rule name initStateIdx states


-- States list materializer

instance
  MaterializeList () (StatesTag '[]) [CustomStateVL] where
  matList _ _ = []

instance
  ( Materialize () s CustomStateVL
  , MaterializeList () (StatesTag ss) [CustomStateVL]
  ) =>
  MaterializeList () (StatesTag (s ': ss)) [CustomStateVL] where
  matList _ _ = mat () (Proxy @s) : matList () (Proxy @(StatesTag ss))


-- State materializer

instance
  ( KnownNat idx
  , KnownSymbol name
  , MaterializeList () (ConditionsTag conds) [CustomConditionVL]
  ) =>
  Materialize () ('State idx name conds) CustomStateVL where
  mat _ _ = let
    idx = fromIntegral $ natVal $ Proxy @idx
    name = symbolVal $ Proxy @name
    conds = matList () $ Proxy @(ConditionsTag conds)
    in State idx name conds

instance
  ( KnownNat idx
  , KnownSymbol name
  ) =>
  Materialize () ('FinishState idx name) CustomStateVL where
  mat _ _ = let
    idx = fromIntegral $ natVal $ Proxy @idx
    name = symbolVal $ Proxy @name
    in FinishState idx name


-- Conditions list materializer

instance
  MaterializeList () (ConditionsTag '[]) [CustomConditionVL] where
  matList _ _ = []

instance
  ( Materialize () c CustomConditionVL
  , MaterializeList () (ConditionsTag cs) [CustomConditionVL]
  ) =>
  MaterializeList () (ConditionsTag (c ': cs)) [CustomConditionVL] where
  matList _ _ = mat () (Proxy @c) : matList () (Proxy @(ConditionsTag cs))


-- Condition materializer

instance
  ( Materialize () writeAct CustomWriteActionVL
  , Materialize () moveAct CustomMoveHeadActionVL
  , KnownNat nextStateIdx
  , KnownSymbol symb
  ) =>
  Materialize ()
    ('Match symb writeAct moveAct nextStateIdx)
    CustomConditionVL
  where
    mat _ _ = let
      symb = case symbolVal $ Proxy @symb of
        [] -> error "Symbol to match can't be empty."
        (ch:_) -> ch
      nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
      writeAct = mat () $ Proxy @writeAct
      moveAct = mat () $ Proxy @moveAct
      in Match symb writeAct moveAct nextStateIdx

instance
  ( Materialize () writeAct CustomWriteActionVL
  , Materialize () moveAct CustomMoveHeadActionVL
  , KnownNat nextStateIdx
  ) =>
  Materialize ()
    ('MatchAny writeAct moveAct nextStateIdx)
    CustomConditionVL
  where
    mat _ _ = let
      nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
      writeAct = mat () $ Proxy @writeAct
      moveAct = mat () $ Proxy @moveAct
      in MatchAny writeAct moveAct nextStateIdx

instance
  ( Materialize () writeAct CustomWriteActionVL
  , Materialize () moveAct CustomMoveHeadActionVL
  , KnownNat nextStateIdx
  ) =>
  Materialize ()
    ('MatchBlank writeAct moveAct nextStateIdx)
    CustomConditionVL
  where
    mat _ _ = let
      nextStateIdx = fromIntegral $ natVal $ Proxy @nextStateIdx
      writeAct = mat () $ Proxy @writeAct
      moveAct = mat () $ Proxy @moveAct
      in MatchBlank writeAct moveAct nextStateIdx

instance
  ( KnownSymbol msg
  ) =>
  Materialize () ('FailWith msg) CustomConditionVL where
    mat _ _ = let
      msg = symbolVal $ Proxy @msg
      in FailWith msg


-- Write action materializer

instance
  ( KnownSymbol symb
  ) =>
  Materialize () ('Write symb) CustomWriteActionVL where
  mat _ _ = case symbolVal $ Proxy @symb of
    [] -> WriteBlank
    (ch:_) -> Write ch

instance
  Materialize () 'WriteMatched CustomWriteActionVL where
  mat _ _ = WriteMatched

instance
  Materialize () 'WriteBlank CustomWriteActionVL where
  mat _ _ = WriteBlank

instance
  Materialize () 'Skip CustomWriteActionVL where
  mat _ _ = Skip


-- Move action materializer

instance
  Materialize () 'L CustomMoveHeadActionVL where
  mat _ _ = L

instance
  ( KnownNat n
  ) =>
  Materialize () ('Ln n) CustomMoveHeadActionVL where
  mat _ _ = Ln $ fromIntegral $ natVal $ Proxy @n

instance
  Materialize () 'R CustomMoveHeadActionVL where
  mat _ _ = R

instance
  ( KnownNat n
  ) =>
  Materialize () ('Rn n) CustomMoveHeadActionVL where
  mat _ _ = Rn $ fromIntegral $ natVal $ Proxy @n

instance
  Materialize () 'Stay CustomMoveHeadActionVL where
  mat _ _ = Stay





-- instance
--   ( RuleRunner rule
--   , rule ~ 'Rule name idx ss
--   , KnownSymbol name
--   ) =>
--   IMachine () rule where
--   run () = runRule
--   name () _ = symbolVal $ Proxy @name


-- class RuleRunner (rule :: CustomRule 'TypeLevel) where
--   runRule
--     :: Proxy rule
--     -> Tape
--     -> Either String Tape

-- class RuleRunnerImpl (rule :: CustomRule 'TypeLevel) where
--   runRule'
--     :: Proxy rule
--     -> CurrentStateIdx
--     -> Tape
--     -> Either String Tape


-- -- | States recursive runner.

-- -- | Applies the state if it matches the current state.
-- -- N.B. This is the overlapping instances workaround.
-- --   We can't have 2 instances of RuleRunner:
-- -- instance
-- --   ( (curState == stIdx) ~ 'True
-- --   ) =>
-- --   RuleRunner ...
-- --
-- -- instance
-- --   ( (curState == stIdx) ~ 'False
-- --   ) =>
-- --   RuleRunner ...
-- --
-- -- These instances overlap.
-- -- Moving the matching of the states to the value level solves this.

-- class StatesRunner (states :: [CustomState 'TypeLevel]) where
--   runStates
--     :: Proxy states
--     -> CurrentStateIdx
--     -> Tape
--     -> Result


-- -- | Transition conditions runner.

-- class Materialize (conds :: [CustomCondition 'TypeLevel]) where
--   runConditions
--     :: Proxy conds
--     -> Tape
--     -> Result


-- -- | Writing to tape action runner.

-- class WriteActionRunner (writeAct :: CustomWriteAction 'TypeLevel) where
--   runWrite
--     :: Proxy writeAct
--     -> TapeSymbol
--     -> Tape
--     -> Tape


-- -- | Moving the tape head runner.

-- class MoveActionRunner (moveAct :: CustomMoveHeadAction 'TypeLevel) where
--   runMove
--     :: Proxy moveAct
--     -> Tape
--     -> Tape

-- --------------- Implementation ---------------

-- -- Initial runner.

-- -- No states provided - finishing.
-- instance
--   RuleRunner ('Rule n stIdx '[]) where
--   runRule _ tape = Right tape

-- -- Starting the run with the init state idx as a current state
-- instance
--   ( KnownNat initStateIdx
--   , RuleRunnerImpl ('Rule name initStateIdx (s ': ss))
--   ) =>
--   RuleRunner ('Rule name initStateIdx (s ': ss)) where
--   runRule ruleProxy tape = let
--     initStateIdx = fromIntegral $ natVal $ Proxy @initStateIdx
--     in runRule' ruleProxy initStateIdx tape


-- -- Actual step-by-step recursive rule runner.

-- instance
--   ( StatesRunner states
--   ) =>
--   RuleRunnerImpl ('Rule n stIdx states) where
--   runRule' ruleProxy curStateIdx tape1 = let
--     res = runStates (Proxy @states) curStateIdx tape1
--     in case res of
--       FailedWith errMsg -> Left $ "[" <> show curStateIdx <> "] " <> errMsg
--       Finished tape2 -> Right tape2
--       Successful nextStateIdx tape2 ->
--         runRule' ruleProxy nextStateIdx tape2

