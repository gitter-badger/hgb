module Eval where

import Grammar

evalInt :: Expression -> Int
evalInt (Number n) = n
evalInt (Call "+" [x]) = evalInt x
evalInt (Call "-" [x]) = -evalInt x
evalInt (Call function args) =
  case function of
    "+" -> a + b
    "-" -> a - b
    "*" -> a * b
    "/" -> a `div` b
    "%" -> a `mod` b
  where
    [a, b] = map evalInt args
