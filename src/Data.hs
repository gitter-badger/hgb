module Data where

import Data.List hiding (take, drop)
import Data.Set hiding (take, drop)
import Data.Char hiding (take, drop)
import Data.Map
import Symbol

data Token =    Token Symbol String
                deriving (Eq, Show, Read)

hgbLex :: String -> [Token]
hgbLex "" = []
hgbLex all@(x:xs)
    | isSpace(x)            = hgbLex xs
    | [x] == strBoundStr    = lexStr all
    | isDigit x             = lexDigit all
    | isAlpha x             = lexAlphaKeyword all
    | otherwise             = lexNonAlphaKeyWord all


lexStr :: String -> [Token]
lexStr all@(delim:xs) = (Token StrBoundSymbol [delim]) : lexStr' "" xs

lexStr' :: String -> String -> [Token]
lexStr' soFar (x:xs)
  | x == delim =
    (Token StringSymbol soFar) : (Token StrBoundSymbol [delim]) : hgbLex xs
  | otherwise = lexStr' (soFar ++ [x]) xs

lexDigit :: String -> [Token]
lexDigit str = lexDigit' "" str
    where
        lexDigit' :: String -> String -> [Token]
        lexDigit' soFar all@(x:xs)
            | isDigit x = lexDigit' (soFar ++ [x]) xs
            | otherwise = [Token DigitSymbol soFar] ++ hgbLex xs

lexAlphaKeyword :: String -> [Token]
lexAlphaKeyword = lexByPredicate (\x -> not (isAlpha x || isDigit))

lexNonAlphaKeyWord :: String -> [Token]
lexNonAlphaKeyWord = lexByPredicate (\x -> isSpace x || isAlpha x || isDigit x)

lexByPredicate :: (String -> Bool) -> String -> [Token]
lexByPredicate = lexByPredicate' ""

lexByPredicate' :: String -> String -> [Token]
lexByPredicate' accumulator all@(x:xs)
    | predicate x   = (Token symbolType accumulator) : hgbLex xs
                        where symbolType = strToSymbol accumulator
    | otherwise     = lexByPredicate' xs (soFar ++ [x])
