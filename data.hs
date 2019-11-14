data Symbol =   NEQ | ITER_UPTO | GEQ | LEQ | Eq | DOT | LBRACKET | RBRACKET |
                LPAREN | RPAREN | LBRACE | RBRACE | TYPE_DELIM | VAL_DELIM |
                ASSIGN | GT | LT | PLUS | MINUS | TIMES | DIV | MOD |
                CHAR_BOUND | STR_BOUND | EXPR_END | IN | AND | NOT | OR | FOR |
                WHILE | IF | ELSE | ELSEIF | FUNC | RETURN | TYPE | INVALID
                deriving (Eq, Ord, Show, Read, Bounded)

data Token = Symbol String

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
