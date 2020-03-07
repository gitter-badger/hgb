module Token where

import Symbol (Symbol(..))

data Token =
  Token
    { symbol :: Symbol
    , content :: String
                    -- Refers to the index of the first character of the token in the source code
    , start :: Int
                    -- Refers to the index of the character after the last
    , end :: Int
    }
  deriving (Eq, Show)
