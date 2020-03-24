module Error
  ( Error(..)
  , ErrorType(..)
  , Expectation(..)
  ) where

import Symbol (Symbol(..), symbolToStr)
import Token (Token(..))
import Utils (Span)

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

data Expectation
  = Expression
  | ExpressionTerminator
  | ExpressionOrDelimiter
  | EnumerationListDelimiter [Symbol]
  | CloseToken Symbol

instance Show Expectation where
  show Expression = "an expression"
  show ExpressionTerminator = show (symbolToStr Symbol.ExprEnd)
  show ExpressionOrDelimiter = show Expression ++ " or delimiter"
  show (EnumerationListDelimiter options) = displayOptions (map symbolToStr options)
  show (CloseToken closer) = show (symbolToStr closer)

data ErrorType =
  Expected Expectation Token

instance Show ErrorType where
  show (Expected expectation token) =
    "expected " ++ show expectation ++ ", got " ++ show (content token)

data Error =
  Error ErrorType Span

instance Show Error where
  show (Error errorType span) =
    "Error: " ++ show errorType ++ " at " ++ show span
