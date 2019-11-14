module Data where

import Data.List

data Symbol =   NEQ | ITER_UPTO | GEQ | LEQ | Eq | DOT | LBRACKET | RBRACKET |
                LPAREN | RPAREN | LBRACE | RBRACE | TYPE_DELIM | VAL_DELIM |
                ASSIGN | GT | LT | PLUS | MINUS | TIMES | DIV | MOD |
                CHAR_BOUND | STR_BOUND | EXPR_END | IN | AND | NOT | OR | FOR |
                WHILE | IF | ELSE | ELSEIF | FUNC | RETURN | TYPE | INVALID | STRING
                deriving (Eq, Show, Read, Bounded)

data Token = Symbol String

addToken :: (Symbol sym) => (sym, String) -> (sym, String) -> (sym, String)
addToken (sym, s1) (sym, s2) = (sym, s1 ++ s2)

lexBasic :: String -> Symbol

lexBasic word = case word of
                "=/=" -> NEQ
                "->" -> ITER_UPTO
                ">=" -> GEQ
                "<=" -> LEQ
                "==" -> EQ
                "." -> DOT
                "[" -> LBRACKET
                "]" -> RBRACKET
                "(" -> LPAREN
                ")" -> RPAREN
                "{" -> LBRACE
                "}" -> RBRACE
                ":" -> TYPE_DELIM
                "," -> VAL_DELIM
                "=" -> ASSIGN
                ">" -> GT
                "<" -> LT
                "+" -> PLUS
                "-" -> MINUS
                "*" -> TIMES
                "/" -> DIV
                "%" -> MOD
                "\'" -> CHAR_BOUND
                "\"" -> STR_BOUND
                "!" -> EXPR_END
                "in" -> IN
                "and" -> AND
                "not" -> NOT
                "or" -> OR
                "for" -> FOR
                "while" -> WHILE
                "if" -> IF
                "else" -> ELSE
                "elseif" -> ELSEIF
                "fun" -> FUNC
                "return" -> RETURN
                "int8" -> TYPE
                "int16" -> TYPE
                "int32" -> TYPE
                "int64" -> TYPE
                "int" -> TYPE
                "uint8" -> TYPE
                "uint16" -> TYPE
                "uint32" -> TYPE
                "uint64" -> TYPE
                "uint" -> TYPE
                "char" -> TYPE
                "void" -> TYPE
                s -> NAME


strDelim = "\""

hgbLex :: String -> [Token]
hgbLex "" = []
hgbLex all@(x:xs)
    | isWhiteSpace(x)   = hgbLex xs
    | x == strDelim     = lexStr xs
    | isAlpha(x)        = lexKeyword all
    | otherwise         = lexBuiltInOperator all


isStringDelim:: Char -> Bool
isStringDelim = (=="\"")

isWhiteSpace :: Char -> Bool
isWhiteSpace c = elem c " \t\n"

lexStr :: String -> [Token]
lexStr str
    let strLen = elemIndex strDelim str in
    | strLen == Nothing   = error "Error in code: String opened without closing"
    | otherwise           = [(STRING, strContent)] ++ hgbLex remainder
                            where   strContent = take strLen str
                                    remainder = drop (strLen + 1) str

lexKeyword :: String -> [Token]
lexKeyword str =
    let wl = wordLen str
        word = take wl str
        remainder = drop wl str
    in [(identifyKeyWord word, word)] ++ hgbLex remainder

wordLen :: String -> Int
