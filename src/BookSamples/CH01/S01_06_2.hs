import Data.List (intercalate)

-- "imaginary-sql" library space

data SqlClause a = Select String | From String | UserDefined a

generateSql :: Show a => [SqlClause a] -> String
generateSql clauses = intercalate " " $ map showClause clauses
  where
    showClause (Select a) = "SELECT " ++ a
    showClause (From a) = "FROM " ++ a
    showClause (UserDefined a) = show a

-- User space

data MyClause = Where String

instance Show MyClause where
  show (Where s) = "WHERE " ++ s

myQuery :: String
myQuery = generateSql [Select "abc", From "table", UserDefined (Where "cde")]
