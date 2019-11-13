

data Symbol =   NEQ | ITER_UPTO | GEQ | LEQ | Eq | DOT | LBRACKET | RBRACKET |
                LPAREN | RPAREN | LBRACE | RBRACE | TYPE_DELIM | VAL_DELIM |
                ASSIGN | GT | LT | PLUS | MINUS | TIMES | DIV | MOD |
                CHAR_BOUND | STR_BOUND | EXPR_END | IN | AND | NOT | OR | FOR |
                WHILE | IF | ELSE | ELSEIF | FUNC | RETURN | TYPE | INVALID
                deriving (Eq, Ord, Show, Read, Bounded)

data Token = Symbol String

lexBasic :: String -> Symbol

lexBasic "=/=" = NEQ
-- lexBasic "->" = ITER_UPTO
-- lexBasic ">=" = GEQ
-- lexBasic "<=" = LEQ
-- lexBasic "==" = EQ
lexBasic "." = DOT
lexBasic "[" = LBRACKET
lexBasic "]" = RBRACKET
lexBasic "(" = LPAREN
lexBasic ")" = RPAREN
lexBasic "{" = LBRACE
lexBasic "}" = RBRACE
lexBasic ":" = TYPE_DELIM
lexBasic "," = VAL_DELIM
-- lexBasic "=" = ASSIGN
-- lexBasic ">" = GT
-- lexBasic "<" = LT
lexBasic "+" = PLUS
lexBasic "-" = MINUS
lexBasic "*" = TIMES
lexBasic "/" = DIV
lexBasic "%" = MOD
lexBasic "\'" = CHAR_BOUND
lexBasic "\"" = STR_BOUND
lexBasic "!" = EXPR_END
lexBasic "in" = IN
lexBasic "and" = AND
lexBasic "not" = NOT
lexBasic "or" = OR
lexBasic "for" = FOR
lexBasic "while" = WHILE
lexBasic "if" = IF
lexBasic "else" = ELSE
lexBasic "elseif" = ELSEIF
lexBasic "fun" = FUNC
lexBasic "return" = RETURN
lexBasic "int8" = TYPE
lexBasic "int16" = TYPE
lexBasic "int32" = TYPE
lexBasic "int64" = TYPE
lexBasic "int" = TYPE
lexBasic "uint8" = TYPE
lexBasic "uint16" = TYPE
lexBasic "uint32" = TYPE
lexBasic "uint64" = TYPE
lexBasic "uint" = TYPE
lexBasic "char" = TYPE
lexBasic "void" = TYPE
lexBasic s = NAME

isWhiteSpace :: Char -> Bool
isWhiteSpace c = elem c " \t\n"


lex_:: String -> [Token]
lex_"" = []
lex_ all@(x:xs)
    | isWhiteSpace(x)           = lex_ xs
    | lb == STR_BOUND           = lexStr xs
    | lb == INVALID                 = [lexBasic (fw)] ++ lex_ (drop (length fw + 1) all)
    where   fw = firstWord all
            lb = lexBasic fw


lexStr :: String -> [Token]
lexStr s =
