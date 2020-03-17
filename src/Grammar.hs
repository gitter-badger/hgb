module Grammar (Expression(..)) where

data Expression
  = Number Int
  | String String
  | Variable String
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
  deriving (Eq)

instance Show Expression where
  show (Number n) = show n
  show (String s) = show s
  show (Variable v) = "$" ++ v
  show (Call name arguments) =
    "(" ++ name ++ " " ++ unwords (map show arguments) ++ ")"
