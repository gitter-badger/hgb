data Expressions = Expr | Expr Expressions
data Expr = Expr Op Expr | Paren Expr Paren
