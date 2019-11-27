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
    | isDigit x             = lexNumber all
    | isAlpha x             = lexAlphaKeyword all
    | otherwise             = lexNonAlphaKeyWord all


lexStr :: String -> [Token]
lexStr (delim:xs) =
    (Token StrBoundSymbol [delim]) : (Token StringSymbol content)
        : (Token StrBoundSymbol [delim]) : hgbLex remainder
    where (content, (delim:remainder)) = span (/= delim) xs

lexStr' :: String -> String -> [Token]
lexStr' soFar (x:xs)
  | [x] == strBoundStr =
    (Token StringSymbol soFar) : (Token StrBoundSymbol [x]) : hgbLex xs
  | otherwise = lexStr' (soFar ++ [x]) xs

lexNumber :: String -> [Token]
lexNumber str =  (Token DigitSymbol dig) : hgbLex remainder
                where (dig, remainder) = span isDigit str

lexAlphaKeyword :: String -> [Token]
lexAlphaKeyword str =
    (Token (strToSymbol keyWord) keyWord) : hgbLex remainder
    where (keyWord, remainder) = span isAlphaNum str

lexNonAlphaKeyWord :: String -> [Token]
lexNonAlphaKeyWord str =
    (Token (strToSymbol keyWord) keyWord) : hgbLex remainder
    where (keyWord, remainder) = span (\x -> not (isAlphaNum x|| isSpace x)) str
