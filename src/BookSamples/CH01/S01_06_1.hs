module BookSamples.CH01.S01_06_1 where

import Data.List (intercalate)

-- "imaginary-sql" library space

data SqlClause = Select String | From String

generateSql :: [SqlClause] -> String
generateSql = intercalate " " . map showClause
  where
    showClause (Select a) = "SELECT " ++ a
    showClause (From a) = "FROM " ++ a



-- User space

myQuery :: String
myQuery = generateSql [Select "abc", From "table"]
