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

instance Show Expectation where
  show Expression = "an expression"
  show Operator = "an operator"
  show (Symbol Name) = "a name"
  show (Symbol sym) = show $ symbolToStr sym
  show Assignment = "an assignment"

data ErrorType
  = Expected [Expectation] Token
  | UnterminatedLiteral Symbol
  | CharLiteralMustContainOneCodepoint

instance Show ErrorType where
  show (Expected expectations token) =
    "expected " ++
    displayOptions expectations ++ ", got " ++ show (content token)
  show (UnterminatedLiteral kind) = do
    let kindStr =
          case kind of
            Symbol.String -> "string"
            Symbol.Char -> "char"
    "unterminated " ++ kindStr ++ " literal"
  show CharLiteralMustContainOneCodepoint =
    "character literal must contain exactly one codepoint"

data Error =
  Error ErrorType Span

instance Show Error where
  show (Error errorType span) =
    "Error: " ++ show errorType ++ " at " ++ show span
