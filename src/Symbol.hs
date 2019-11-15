module Symbol where

import Data.List
import Data.Set

neqStr = "=/="
iterUptoStr = ">"
geqStr = ">="
leqStr = "<="
eqStr = "=="
dotStr = "."
lbracketStr = "["
rbracketStr = "]"
lparenStr = "("
rparenStr = ")"
lbraceStr = "{"
rbraceStr = "}"
typeDelimStr = ":"
valDelimStr = ","
assignStr = "="
gtStr = ">"
ltStr = "<"
plusStr = "+"
minusStr = ""
timesStr = "*"
divStr = "/"
modStr = "%"
charBoundStr = "\'"
strBoundStr = "\""
exprEndStr = "!"
inStr = "in"
andStr = "and"
notStr = "not"
orStr = "or"
forStr = "for"
whileStr = "while"
ifStr = "if"
elseStr = "else"
elseifStr = "elseif"
funcStr = "fun"
returnStr = "return"
int8Str = "int8"
int16Str = "int16"
int32Str = "int32"
int64Str = "int64"
intStr = "int"
uint8Str = "uint8"
uint16Str = "uint16"
uint32Str = "uint32"
uint64Str = "uint64"
uintStr = "uint"
charStr = "char"
voidStr = "void"

operatorStrs =  [ neqStr, iterUptoStr, geqStr,
                    leqStr, eqStr, dotStr, lbracketStr,
                    rbracketStr, lparenStr, rparenStr,
                    lbraceStr, rbraceStr, typeDelimStr,
                    valDelimStr, assignStr, gtStr,
                    ltStr, plusStr, minusStr, timesStr,
                    divStr, modStr, charBoundStr,
                    strBoundStr, exprEndStr ]

operatorStrsSet = fromList operatorStrs

typeStrsSet = fromList [ int8Str, int16Str, int32Str, int64Str,
                        intStr, uint8Str, uint16Str, uint32Str,
                        uint64Str, uintStr, charStr, voidStr ]

operatorDelimsSet = fromList [head sym | sym <- operatorStrs]

data Symbol =   NeqSymbol | IterUptoSymbol | GeqSymbol | LeqSymbol | EqSymbol |
                DotSymbol | LbracketSymbol | RbracketSymbol | LparenSymbol |
                RparenSymbol | LbraceSymbol | RbraceSymbol | TypeDelimSymbol |
                ValDelimSymbol | AssignSymbol | GtSymbol | LtSymbol |
                PlusSymbol | MinusSymbol | TimesSymbol | DivSymbol | ModSymbol |
                CharBoundSymbol | StrBoundSymbol | ExprEndSymbol | InSymbol |
                AndSymbol | NotSymbol | OrSymbol | ForSymbol | WhileSymbol |
                IfSymbol | ElseSymbol | ElseifSymbol | FuncSymbol |
                ReturnSymbol | TypeSymbol | InvalidSymbol | StringSymbol |
                DigitSymbol | NameSymbol
                deriving (Eq, Enum, Show, Read)
