module Error
  ( Error(..)
  , ErrorType(..)
  ) where

import Symbol (Symbol(..))
import Token (Token)

displayOptions :: Show a => [a] -> String
displayOptions [one] = show one -- Option
displayOptions [first, second] = show first ++ " or " ++ show second -- Option or AnotherOption
displayOptions all = concat all -- Option, AnotherOption, ..., or YetAnotherOption
  where
    concat :: Show a => [a] -> String
    concat lst =
      case lst
      -- ..., AnotherOption, or YetAnotherOption
            of
        [slast, last] -> show slast ++ ", or " ++ show last
      -- Option, ...
        (first:rest) -> show first ++ ", " ++ concat rest

data ErrorType =
  ExpectedSymbol
    { expected :: [Symbol]
    , actual :: Symbol
    }

instance Show ErrorType where
  show (ExpectedSymbol expected actual) =
    "expected " ++
    displayOptions expected ++ "; got " ++ displayOptions [actual]

-- TODO(ayazhafiz): include the location of an error
newtype Error =
  Error ErrorType

instance Show Error where
  show (Error errorType) = "Error: " ++ show errorType
