module Turing.App.Storage where

import Turing.Machine.Interface (IMachine)
import Turing.Machine.Language

import qualified Data.Map as Map


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


-- data RuleImpl where
--   RI
--     :: IAutomaton () rule
--     => Proxy rule
--     -> RuleImpl
--   DynRI
--     :: IAutomaton DynamicRule 'DynRule
--      => DynamicRule
--      -> RuleImpl

-- type Rules = Map.Map RuleCode RuleImpl


-- getCode :: RuleImpl -> RuleCode
-- getCode (RI proxy) = getCode' proxy
-- getCode (DynRI dynRule) = code dynRule (Proxy @'DynRule)

-- getCode' :: IAutomaton () rule => Proxy rule -> RuleCode
-- getCode' proxy = code () proxy

