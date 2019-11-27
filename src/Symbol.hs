module Symbol where

import Data.List
import Data.Set
import Data.Map

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

strToSymbol :: String -> Symbol
strToSymbol "neqStr" = NeqSymbol
strToSymbol "iterUptoStr" = IterUptoSymbol
strToSymbol "geqStr" = GeqSymbol
strToSymbol "leqStr" = LeqSymbol
strToSymbol "eqStr" = EqSymbol
strToSymbol "dotStr" = DotSymbol
strToSymbol "lbracketStr" = LbracketSymbol
strToSymbol "rbracketStr" = RbracketSymbol
strToSymbol "lparenStr" = LparenSymbol
strToSymbol "rparenStr" = RparenSymbol
strToSymbol "lbraceStr" = LbraceSymbol
strToSymbol "rbraceStr" = RbraceSymbol
strToSymbol "typeDelimStr" = TypeDelimSymbol
strToSymbol "valDelimStr" = ValDelimSymbol
strToSymbol "assignStr" = AssignSymbol
strToSymbol "gtStr" = GtSymbol
strToSymbol "ltStr" = LtSymbol
strToSymbol "plusStr" = PlusSymbol
strToSymbol "minusStr" = MinusSymbol
strToSymbol "timesStr" = TimesSymbol
strToSymbol "divStr" = DivSymbol
strToSymbol "modStr" = ModSymbol
strToSymbol "charBoundStr" = CharBoundSymbol
strToSymbol "strBoundStr" = StrBoundSymbol
strToSymbol "exprEndStr" = ExprEndSymbol
strToSymbol "inStr" = InSymbol
strToSymbol "andStr" = AndSymbol
strToSymbol "notStr" = NotSymbol
strToSymbol "orStr" = OrSymbol
strToSymbol "forStr" = ForSymbol
strToSymbol "whileStr" = WhileSymbol
strToSymbol "ifStr" = IfSymbol
strToSymbol "elseStr" = ElseSymbol
strToSymbol "elseifStr" = ElseifSymbol
strToSymbol "funcStr" = FuncSymbol
strToSymbol "returnStr" = ReturnSymbol
strToSymbol "int8Str" = TypeSymbol
strToSymbol "int16Str" = TypeSymbol
strToSymbol "int32Str" = TypeSymbol
strToSymbol "int64Str" = TypeSymbol
strToSymbol "intStr" = TypeSymbol
strToSymbol "uint8Str" = TypeSymbol
strToSymbol "uint16Str" = TypeSymbol
strToSymbol "uint32Str" = TypeSymbol
strToSymbol "uint64Str" = TypeSymbol
strToSymbol "uintStr" = TypeSymbol
strToSymbol "charStr" = TypeSymbol

strToSymbol str = NameSymbol
