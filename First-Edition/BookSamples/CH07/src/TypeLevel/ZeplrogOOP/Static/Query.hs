{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module TypeLevel.ZeplrogOOP.Static.Query where

import CPrelude

import TypeLevel.ZeplrogOOP.Static.Model


getComboPropertyId :: PropertyGroupVL -> (EssenceVL, StaticPropertyId)
getComboPropertyId (GroupId ess sId)       = (ess, sId)
getComboPropertyId (GroupRootId ess sId _) = (ess, sId)

getEssenceFromKV :: PropertyKeyValueVL -> EssenceVL
getEssenceFromKV (PropKeyBag ess _) = ess
getEssenceFromKV (PropKeyVal ess _) = ess

getGroup :: PropertyVL -> PropertyGroupVL
getGroup (PropDict group _ _) = group
getGroup (TagPropRef _) = error "getGroup not implemented for TagPropRef"

getTagPropEss :: TagPropertyVL -> EssenceVL
getTagPropEss (TagProp tagGroup) = getTagPropGroupEssence tagGroup

getTagPropGroupEssence :: TagPropertyGroupVL -> EssenceVL
getTagPropGroupEssence (TagGroup ess) = ess
getTagPropGroupEssence (TagGroupRoot ess _) = ess

getStringValue :: ValDefVL -> Maybe String
getStringValue (StringValue str) = Just str
getStringValue _ = Nothing


-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValue :: EssencePathVL -> PropertyVL -> Maybe String
queryStringValue [] _ = Nothing
queryStringValue _ (TagPropRef _) =
  error "queryStringValue not implemented for TagPropRef"
queryStringValue (ess:esss) (PropDict group kvs _) = let
  (ess', _) = getComboPropertyId group
  in case ess == ess' of
        True -> queryStringValueForKeyVals esss kvs
        False -> Nothing

-- Hardcoded function.
-- Queries a value of string for this property.
-- The path doesn't contain the essence of this property.
-- TODO: move to the Query language.
queryStringValueRelative :: EssencePathVL -> PropertyVL -> Maybe String
queryStringValueRelative [] _ = Nothing
queryStringValueRelative _ (TagPropRef _) =
  error "queryStringValueRelative not implemented for TagPropRef"
queryStringValueRelative esss (PropDict group kvs _) =
  queryStringValueForKeyVals esss kvs

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForKeyVals
  :: EssencePathVL
  -> [PropertyKeyValueVL]
  -> Maybe String
queryStringValueForKeyVals [] _ = Nothing
queryStringValueForKeyVals _ [] = Nothing
queryStringValueForKeyVals path@(ess:_) (PropKeyVal ess' owning : kvs)
  | ess == ess' = queryStringValueForOwning path owning
  | otherwise = queryStringValueForKeyVals path kvs
-- queryStringValueForKeyVals path@(ess:_) (PropKeyBag ess' props : kvs)
--   | ess == ess' = queryStringValueForProps path props
--   | otherwise = queryStringValueForKeyVals path kvs
queryStringValueForKeyVals _ _ = error "queryStringValueForKeyVals not implemented"

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForOwning
  :: EssencePathVL
  -> PropertyOwningVL
  -> Maybe String
queryStringValueForOwning esss (OwnVal valDef) =
  getStringValue valDef
queryStringValueForOwning esss (OwnProp prop) =
  queryStringValue esss prop
queryStringValueForOwning esss (SharedProp prop) =
  queryStringValue esss prop

-- Hardcoded function.
-- TODO: move to the Query language.
queryStringValueForOwnings
  :: EssencePathVL
  -> [PropertyOwningVL]
  -> Maybe String
queryStringValueForOwnings [] _ = Nothing
queryStringValueForOwnings _ [] = Nothing
queryStringValueForOwnings esss (owning : ownings) =
  case queryStringValueForOwning esss owning of
    Nothing  -> queryStringValueForOwnings esss ownings
    Just str -> Just str
