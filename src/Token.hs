module Token where

import Symbol (Symbol(..))

type IndexString = [Index Char]
data Index a = Index a Int

instance Functor Index where
  fmap f (Index a i) = Index (f a) i

data Token =
  Token
    { symbol :: Symbol
    , content :: IndexString
    }
  deriving (Eq, Show)
