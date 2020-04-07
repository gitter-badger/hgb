module Grammar
  ( Expression(..)
  , Bits(..)
  , Type(..)
  ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Read (readMaybe)

data Bits
  = B8
  | B16
  | B32
  | B64
  deriving (Eq)

bitsTable = [(B8, 8), (B16, 16), (B32, 32), (B64, 64)]

instance Enum Bits where
  fromEnum bit = fromJust $ lookup bit bitsTable
  toEnum bit = fromJust $ lookup bit (map swap bitsTable)

instance Show Bits where
  show = show . fromEnum

instance Read Bits where
  readsPrec _ bits = [(toEnum (read bits :: Int), "")]

data Type
  = Int
      { numBits :: Maybe Bits
      , signed :: Bool
      }
  | Double
  | Void
  | Array Type
  deriving (Eq)

instance Show Type where
  show Double = "double"
  show Void = "void"
  show (Int (Just n) False) = "uint" ++ show n
  show (Int Nothing False) = "uint"
  show (Int (Just n) True) = "int" ++ show n
  show (Int Nothing True) = "int"
  show (Array ty) = "[" ++ show ty ++ "]"

instance Read Type where
  readsPrec _ ty = [(readType ty, "")]
    where
      readType :: String -> Type
      readType "void" = Void
      readType "double" = Double
      readType "int" = Int Nothing True
      readType "uint" = Int Nothing False
      readType ('i':'n':'t':maybeBits) =
        Int (readMaybe maybeBits :: Maybe Bits) True
      readType ('u':'i':'n':'t':maybeBits) =
        Int (readMaybe maybeBits :: Maybe Bits) False

data Expression
  = Number String
  | String String
  | Reference String
  | Declaration
      -- TODO: `name` should be a `Reference`.
      { name :: String
      , ty :: Type
      , maybeAssignment :: Maybe Expression
      }
  | Assignment
      { name :: String
      , assignment :: Expression
      }
  | FunctionDeclaration
      { name :: String
      , params :: [Expression]
      , ty :: Type
      , exprs :: [Expression]
      }
  | Call
      -- Represents a call to a function.
      -- hgb uses a call to represent both prefix and infix function calls. For example,
      --   |-------------|-------------|
      --   | hgb program | `show call` |
      --   |-------------|-------------|
      --   |    "+1"     |    (+ 1)    |
      --   | "foo(1, 2)" |  (foo 1 2)  |
      --   |-------------|-------------|
      { name :: String
      , arguments :: [Expression]
      }
  | Return Expression
  deriving (Eq)

displayList :: Show a => [a] -> String
displayList = unwords . map show

instance Show Expression where
  show (Number n) = n
  show (String s) = show s
  show (Reference v) = "$" ++ v
  show (Call name arguments) =
    "(" ++ name ++ " " ++ displayList arguments ++ ")"
  show (FunctionDeclaration name params type' exprs) =
    "(fun (" ++
    name ++
    " (" ++
    displayList params ++
    ") : " ++ show type' ++ ") (" ++ displayList exprs ++ "))"
  show (Declaration name type' Nothing) =
    "(declare (" ++ show (Reference name) ++ " : " ++ show type' ++ "))"
  show (Declaration name type' (Just expr)) =
    "(declare (" ++
    show (Reference name) ++ " : " ++ show type' ++ ") " ++ show expr ++ ")"
  show (Assignment name expr) =
    "(= " ++ show (Reference name) ++ " " ++ show expr ++ ")"
  show (Return expr) = "(return " ++ show expr ++ ")"
