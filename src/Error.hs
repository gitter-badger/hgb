module Error
  ( Error(..)
  , ErrorType(..)
  , Expectation(..)
  ) where

import Prelude
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
        first:rest -> show first ++ ", " ++ concat rest

data Expectation
  = Expression
  | Operator
  | Symbol Symbol
  | Assignment
  | TypeDeclaration
  | Options [Expectation]

instance Show Expectation where
  show Expression = "an expression"
  show Operator = "an operator"
  show (Symbol Name) = "a name"
  show (Symbol sym) = show $ symbolToStr sym
  show Assignment = "an assignment"
  show TypeDeclaration = "a type declaration"
  show (Options expectations) = displayOptions expectations

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
