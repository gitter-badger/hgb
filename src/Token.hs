module Token where

import Symbol (Symbol(..))

data Token = Token  { symbol :: Symbol
                    , content :: String
                    } deriving (Eq, Show)
