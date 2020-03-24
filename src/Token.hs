module Token where

import Symbol (Symbol)
import Utils (Span)

data Token =
  Token
    { symbol :: Symbol
    , content :: String
    , sourceSpan :: Span -- start and end of the token in the source code
    }
  deriving (Eq, Show)
