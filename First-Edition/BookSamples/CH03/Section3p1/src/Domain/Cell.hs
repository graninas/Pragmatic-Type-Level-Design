module Domain.Cell where

data Cell
  = Alive
  | Dead
  deriving (Show, Eq)


toCell :: Char -> Cell
toCell 'x' = Alive
toCell _ = Dead


