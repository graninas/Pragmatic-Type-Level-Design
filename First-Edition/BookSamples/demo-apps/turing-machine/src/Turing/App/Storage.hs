-- | Types for rules and tapes storage.

module Turing.App.Storage where

import Turing.Machine.Interface
import Turing.Machine.Language

import qualified Data.Map as Map
import Data.Proxy (Proxy)

-- type Generation = Int

-- data WorldInstance where
--   WI
--     :: IAutomaton () rule
--     => Generation
--     -> CellWorld rule
--     -> WorldInstance
--   DynWI
--     :: IAutomaton DynamicRule 'DynRule
--     => DynamicRule
--     -> Generation
--     -> CellWorld 'DynRule
--     -> WorldInstance

-- type WorldIndex = Int
-- type Worlds = Map.Map WorldIndex WorldInstance

type TapeIndex = Int
type Tapes = Map.Map TapeIndex Tape


data RuleImpl where
  RI
    :: IMachine () rule
    => Proxy rule
    -> RuleImpl
  -- DynRI
  --   :: IMachine DynamicRule 'DynRule
  --    => DynamicRule
  --    -> RuleImpl

type Rules = Map.Map String RuleImpl



getName :: RuleImpl -> String
getName (RI proxy) = name () proxy
-- getName (DynRI dynRule) = code dynRule (Proxy @'DynRule)

-- getName' :: IMachine () rule => Proxy rule -> RuleName
-- getName' proxy = name () proxy
