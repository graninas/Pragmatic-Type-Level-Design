{-# LANGUAGE DataKinds #-}
module Common.NonEmptyList where


-- Non-empty list with 1 mandatory element
data CustomList1 a = List1 a [a]

-- Non-empty list with 2 mandatory elements
data CustomList2 a = List2 a a [a]
