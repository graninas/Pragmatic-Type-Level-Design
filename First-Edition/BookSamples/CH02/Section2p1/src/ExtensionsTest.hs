{-# LANGUAGE FlexibleInstances #-}
module ExtensionsTest where



data NoNeedForFlexibleInstances = Test1   -- ADT
type NeedForFlexibleInstances = Int       -- type synonym


class Class a where

instance Class NoNeedForFlexibleInstances where

instance Class NeedForFlexibleInstances where
