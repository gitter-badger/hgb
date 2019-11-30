module Token where

import Symbol (Symbol(..))

data Token =
  Token Symbol String
  deriving (Eq, Show)
