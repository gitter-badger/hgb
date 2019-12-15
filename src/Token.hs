module Token where

import Symbol (Symbol(..))


data Token = Token  { symbol :: Symbol
                    , content :: String
                    --refers to the index of the first character of the token in the source code
                    , start :: Int
                    --refers to the index of the character after the last
                    , end :: Int
                    } deriving (Eq, Show)
