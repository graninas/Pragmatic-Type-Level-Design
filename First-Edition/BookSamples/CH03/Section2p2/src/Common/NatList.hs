{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Common.NatList where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Data.Char (ord)
import qualified Text.Parsec as P
import qualified Control.Applicative as Alt

class ToIntList (ns :: [Nat]) where
  toIntList :: Proxy ns -> [Int]

instance ToIntList '[] where
  toIntList _ = []

instance (KnownNat  c, ToIntList cs) =>
  ToIntList (c ': cs) where
  toIntList _
    = fromIntegral (natVal (Proxy @c))
    : toIntList (Proxy @cs)














data Expr = Lit Int | Plus Expr Expr | Times Expr Expr

type Parser = P.Parsec String ()

isSpace ' ' = True
isDigit '0' = True

space :: Parser Char
space = P.satisfy isSpace

plus :: Parser Char
plus = char '+'

char :: Char -> Parser Char
char c = P.satisfy (== c)

digitValue :: Parser Int
digitValue = fmap digitToInt digit where
  digitToInt c = ord c - ord '0'


digit :: Parser Char
digit = P.satisfy isDigit


optional :: Alt.Alternative f => f a -> f (Maybe a)
optional p = fmap Just p Alt.<|> pure Nothing

-- simpleSumP :: Parser Expr
-- simpleSumP  =
--   do x <- digitValue
--      plus
--      y <- digitValue
--      pure (Plus (Lit x) (Lit y))


-- simpleSumP :: Parser Expr
-- simpleSumP  =
--   do x <- digitValue
--      space
--      plus
--      space
--      y <- digitValue
--      pure (Plus (Lit x) (Lit y))


simpleSumP :: Parser Expr
simpleSumP  =
  do x <- digitValue
     optional space
     plus
     optional space
     y <- digitValue
     pure (Plus (Lit x) (Lit y))


spaces :: Parser ()
spaces = (space >> spaces) Alt.<|> pure ()


many, many1 :: Parser a -> Parser [a]
many  p = many1 p Alt.<|> pure []
many1 p = do x <- p
             xs <- many p
             pure (x:xs)


number :: Parser Int
number = fmap digitsToNumber (many1 digitValue) where
  digitsToNumber :: [Int] -> Int
  digitsToNumber = foldl (\x y -> 10 * x + y ) 0


-- exprP :: Parser Expr
-- exprP = do spaces
--            x <- literalP
--            spaces
--            let r = do plus
--                       y <- exprP
--                       pure (Plus x y)
--            r Alt.<|> (pure x)
--   where literalP :: Parser Expr
--         literalP = fmap Lit number

literalP = Lit <$> number

plusP  = pure Plus <* plus
opP = plusP Alt.<|> timesP
timesP = pure Times <* char '*'

exprP  = termP `P.chainl1` plusP
termP  = literalP `P.chainl1` timesP
