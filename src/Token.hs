module Token where

import Symbol (Symbol(..))

data Index a = Index a Int deriving (Eq)
type IndexString = [Index Char]

val :: Index a -> a
val (Index c _) = c

isOfVal :: (Eq a) => a -> Index a -> Bool
isOfVal x (Index y _) = (x == y)

equalVal :: (Eq a) => Index a -> Index a -> Bool
equalVal x y = (val x == val y)

valList :: [Index a] -> [a]
valList = map val

toString :: IndexString -> String
toString = valList

instance (Show a) => Show (Index a) where
  show (Index a _) = show a

data Token =
  Token
    { symbol :: Symbol
    , content :: IndexString
    }
  deriving (Eq, Show)
