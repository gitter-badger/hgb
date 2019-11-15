module Data where

import Data.List hiding (take, drop)
import Data.Set hiding (take, drop)
import Data.Char hiding (take, drop)
import Symbol

data Token =    Token Symbol String
                deriving (Eq, Show, Read)

hgbLex :: String -> [Token]
hgbLex "" = []
hgbLex all@(x:xs)
    | isSpace(x)            = hgbLex xs
    | [x] == strBoundStr    = lexStr xs
    | isAlpha x             = lexKeyword all
    | isDigit x             = lexDigit all
    | otherwise             = lexNonAlphaOp all



lexStr :: String -> [Token]
lexStr str = [Token StringSymbol strContent] ++ hgbLex remainder
                where   scl = strContentLen str
                        strContent = take scl str
                        remainder = drop (scl + 1) str

strContentLen :: String -> Int
strContentLen (x:xs)
    | x == (strBoundStr !! 0) = 0
    | otherwise             = 1 + strContentLen xs

lexKeyword :: String -> [Token]
lexKeyword str =
    let wl = wordLen str
        word = take wl str
        remainder = drop wl str
    in [Token (identifyKeyWord word) word] ++ hgbLex remainder

wordLen :: String -> Int
wordLen (x:xs)
    | member x operatorDelimsSet  = 0
    | isSpace x                = 0
    | otherwise                     = 1 + wordLen xs


identifyKeyWord :: String -> Symbol
identifyKeyWord word
    | member word typeStrsSet   = TypeSymbol
    | (word == inStr)           = InSymbol
    | (word == andStr)          = AndSymbol
    | (word == notStr)          = NotSymbol
    | (word == orStr)           = OrSymbol
    | (word == forStr)          = ForSymbol
    | (word == whileStr)        = WhileSymbol
    | (word == ifStr)           = IfSymbol
    | (word == elseStr)         = ElseSymbol
    | (word == elseifStr)       = ElseifSymbol
    | (word == funcStr)         = FuncSymbol
    | (word == returnStr)       = ReturnSymbol


lexDigit :: String -> [Token]
lexDigit str = [Token DigitSymbol digStr] ++ hgbLex remainder
                where   ld = lenDigit str
                        digStr = take ld str
                        remainder = drop ld str

lenDigit :: String -> Int
lenDigit (x:xs)
    | isDigit(x)    = 1 + lenDigit xs
    | otherwise     = 0

lexNonAlphaOp :: String -> [Token]
lexNonAlphaOp str = [Token symbolType nonAlphaStr] ++ hgbLex remainder
                    where   lna = lenNonAlpha str
                            nonAlphaStr = take lna str
                            symbolType = identifyNonAlphaOp nonAlphaStr
                            remainder = drop lna str

lenNonAlpha :: String -> Int
lenNonAlpha (x:xs)
    | isSpace x || isAlpha x || isDigit x   = 0
    | otherwise                             = 1 + lenNonAlpha xs

identifyNonAlphaOp :: String -> Symbol
identifyNonAlphaOp str
    | (str == neqStr)          = NeqSymbol
    | (str == iterUptoStr)     = IterUptoSymbol
    | (str == geqStr)          = GeqSymbol
    | (str == leqStr)          = LeqSymbol
    | (str == eqStr)           = EqSymbol
    | (str == dotStr)          = DotSymbol
    | (str == lbracketStr)     = LbracketSymbol
    | (str == rbracketStr)     = RbracketSymbol
    | (str == lparenStr)       = LparenSymbol
    | (str == rparenStr)       = RparenSymbol
    | (str == lbraceStr)       = LbraceSymbol
    | (str == rbraceStr)       = RbraceSymbol
    | (str == typeDelimStr)    = TypeDelimSymbol
    | (str == valDelimStr)     = ValDelimSymbol
    | (str == assignStr)       = AssignSymbol
    | (str == gtStr)           = GtSymbol
    | (str == ltStr)           = LtSymbol
    | (str == plusStr)         = PlusSymbol
    | (str == minusStr)        = MinusSymbol
    | (str == timesStr)        = TimesSymbol
    | (str == divStr)          = DivSymbol
    | (str == modStr)          = ModSymbol
    | (str == charBoundStr)    = CharBoundSymbol
    | (str == strBoundStr)     = StrBoundSymbol
    | (str == exprEndStr)      = ExprEndSymbol
