{-# LANGUAGE MultiWayIf #-}

module Parser
  ( parse
  ) where

import Debug.Trace
import Error (Error(..), ErrorType(..))
import Grammar (Expression)
import qualified Grammar
import Symbol (Symbol(..))
import qualified Symbol
import Token (Token(..))
import Utils

infixOperatorPrecidence :: Symbol -> Int
infixOperatorPrecidence op =
  case op of
    Symbol.RParen -> -1
    Symbol.Minus -> 1
    Symbol.Plus -> 1
    Symbol.Div -> 2
    Symbol.Times -> 2
    Symbol.Mod -> 2

closingDelim :: Symbol -> Symbol
closingDelim s =
  case s of
    Symbol.LParen -> Symbol.RParen

wrapError :: ErrorType -> Either Error any
wrapError err = Left (Error err)

-- This parsing module uses [Top-Down Operator Precedence] parsing (also known as a [Pratt Parsing]
-- technique).  The basic idea is this:
--
-- 1. First, parse prefix operators. These are operators that do not rely on previous parsing
--    contexts. Prefix operators include +/- signs, function calls, and parenthetical expressions. 
--
-- 2. Next, parse infix operators. These are operators that *do* rely on previous parsing contexts.
--    Infix operators include the + and - functions.
-- 
-- [Top-Down Operator Precedence]: https://tdop.github.io/
-- [Pratt Parsing]: https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
parse :: [Token] -> Either Error [Expression]
parse [Token Symbol.EndOfFile _ _ _] = Right []
parse tokens = do
  (expression, afterExpression) <- parseExpression 0 tokens -- parse first expression
  -- Now, parse the rest
  let (tok:rest) = afterExpression
  let sym = symbol tok
  if | sym == Symbol.ExprEnd -> (expression :) <$> parse rest
     | otherwise ->
       wrapError ExpectedSymbol {expected = [Symbol.ExprEnd], actual = sym}

parseExpression :: Int -> [Token] -> Either Error (Expression, [Token])
parseExpression precedence tokens = do
  (left, afterLeft) <- parsePrefix tokens
  parseInfix precedence left afterLeft

-- Parse expressions that do not depend on previous parsing contexts.
parsePrefix :: [Token] -> Either Error (Expression, [Token])
parsePrefix tokens@(tok:_)
  | sym `elem` prefixOperators = parsePrefixCall tokens
  | sym == Symbol.LParen = parseParen tokens
  | sym == Symbol.Number = parseNumber tokens
  | sym == Symbol.Name = parseName tokens
  | sym == Symbol.StrBound = parseString tokens
  | otherwise =
    wrapError
      ExpectedSymbol
        { expected =
            prefixOperators ++
            [Symbol.LParen, Symbol.Number, Symbol.Name, Symbol.StrBound]
        , actual = sym
        }
  where
    prefixOperators = [Symbol.Plus, Symbol.Minus]
    sym = symbol tok

-- Parse expressions that depend on previous parsing contexts.
--
-- Given a parsing context (i.e. a precedence level and parsed LHS expression), this function parses
-- an infix operation and its RHS expression.
--
-- The LHS of the infix expression is the provided parsing context. The RHS of the infix expression
-- is the expression parsed following the infix operator.
--
-- 1. If the next expression on the RHS of the infix operator is of lower or equal precedence to the
--    current parsing context, the parser "gives up" on the current precedence context and returns
--    to the caller the parsed infix expression before the expression it gave up on. This parsed
--    infix expression becomes the new parsing context that the next infix operator N may be parsed
--    with. This creates a parse tree with the infix operator N as the root.
--
-- 2. If the next expression on the RHS of the infix operator I is of higher precedence than the
--    current parsing content, the parser recursively evaluates the rest of the program (starting
--    with the next expression parst the RHS) with the precendence context defined by the RHS
--    expression. This creates a parse tree with the infix operator I as the root.
--
-- >  1 + 2 -> (+ 1 2)
-- >  ^                           | LHS (prefix expression)   precedence = 0
-- >    ^                         | infix operation           precedence = 1
-- >      ^                       | RHS
--
-- >  1 + 2 - 3 -> (- (+ 1 2) 3)
-- >  ^^^^^                       | LHS (infix expression)    precedence = 1
-- >        ^                     | infix operation           precedence = 1
-- >          ^                   | RHS
--
--    After parsing the prefix expression `1`, the parser sees `+` and parses the RHS of the
--    expression starting with `2`. The parser observes that the next expression following the RHS
--    (`-`) has a precendence equal to the current parsing context of `+`, so it stops evaluating
--    the RHS and constructs the sub-tree `1 + 2` as the LHS parsing context for the next infix
--    operator `-`.
--
-- >  1 + 2 * 3 -> (+ 1 (* 2 3))
-- >  ^                           | LHS (prefix expression)   precedence = 0
-- >    ^                         | infix operation           precedence = 1
-- >      ^^^^^                   | RHS (infix expression)    precedence = 2
--
--    After parsing the prefix expression `1`, the parser sees `+` and parses the RHS of the
--    expression starting with `2`. The parser observes that the next expression following the RHS
--    (`*`) has a precedence higher than the current parsing context of `+`, so it recurses into
--    an evaluation of the rest of the program using the RHS of the current expression (`2`) as the
--    LHS parsing context for the parse sub-tree. After being parsed, this sub-tree `2 * 3` becomes
--    the RHS for the `+` expression.
--
-- This strategy enables "nesting" higher-precedence expressions in lower-precendence expressions,
-- preserving an order of operations.
parseInfix :: Int -> Expression -> [Token] -> Either Error (Expression, [Token])
parseInfix precedence left tokens@(operatorTok:afterOperator)
  | operatorSym `elem` expressionTerminators = Right (left, tokens)
  | operatorSym `elem` infixOperators =
    if newPrecidence <= precedence
      then Right (left, tokens)
      else do
        (right, afterExpression) <- parseExpression newPrecidence afterOperator
        let expression = Grammar.Call operatorStr [left, right]
        parseInfix precedence expression afterExpression
  | otherwise =
    wrapError
      ExpectedSymbol
        { expected = infixOperators ++ expressionTerminators
        , actual = operatorSym
        }
  where
    expressionTerminators = [Symbol.ExprEnd, Symbol.ValueDelim]
    infixOperators =
      [ Symbol.Plus
      , Symbol.Minus
      , Symbol.Times
      , Symbol.Div
      , Symbol.Mod
      , Symbol.RParen
      ]
    operatorSym = symbol operatorTok
    newPrecidence = infixOperatorPrecidence operatorSym
    operatorStr = content operatorTok

parseNumber :: [Token] -> Either Error (Expression, [Token])
parseNumber (tok:rest) = Right (Grammar.Number value, rest)
  where
    value = read (content tok) :: Int

parseName :: [Token] -> Either Error (Expression, [Token])
parseName all@(name:listOpen@(Token Symbol.LParen _ _ _):parameterList) =
  parseCall name listOpen parameterList
parseName tokens = parseVariable tokens

parseCall :: Token -> Token -> [Token] -> Either Error (Expression, [Token])
parseCall name listOpen parameterListAndBeyond
  -- Parses a call expression of the form
  --   (data, "value")!
  --   ^                 | list open
  --    ^^^^^^^^^^^^^^^  | parameter list and beyond
 = do
  (arguments, afterArguments) <-
    parseEnumeration listOpen parameterListAndBeyond
  return (Grammar.Call (content name) arguments, afterArguments)

parseEnumeration :: Token -> [Token] -> Either Error ([Expression], [Token])
parseEnumeration
  -- Parses an enumerated list
  --   (data, "value")!
  --   ^                 | list open
  --                 ^   | list close
  --    ^^^^^^^^^^^^^^^  | enumerated list and beyond
 listOpen enumerationAndBeyond@(firstToken:everythingElse)
  | symbol firstToken == listClose = Right ([], everythingElse)
  | otherwise = parseEnumeration' enumerationAndBeyond
  where
    listClose = closingDelim $ symbol listOpen
    parseEnumeration' :: [Token] -> Either Error ([Expression], [Token])
    parseEnumeration' params = do
      (item, afterItem) <- parseExpression 0 params -- parse the first enumerated item
      -- Parse tokens after the enumerated item. There are three cases here:
      --   1. the item is followed by a item delimiter
      --   2. the item is followed by a list close
      --   3. neither (1) nor (2), which means the program is invalid
      let (itemClose:restOfListAndBeyond) = afterItem
      let sym = symbol itemClose
      if | sym == Symbol.ValueDelim
          -- 1. The item is followed by a delimiter
          --    item1, item2, item3)!
          --         ^                 | item close
          --           ^^^^^^^^^^^^^^  | rest of list and beyond
          --    In this case, parse the rest of the enumerated items and append them to the first.
          ->
           do (otherItems, beyondList) <- parseEnumeration' restOfListAndBeyond
              return (item : otherItems, beyondList)
         | sym == listClose
          -- 2. The item is followed by a list close
          --    item)!
          --        ^  | list close
          --         ^ | rest of list and beyond
          --    In this case, just return the one item.
          -> return ([item], restOfListAndBeyond)
         | otherwise
          -- 3. Otherwise, the token is invalid.
          ->
           wrapError
             ExpectedSymbol
               {expected = [Symbol.ValueDelim, listClose], actual = sym}

parseVariable :: [Token] -> Either Error (Expression, [Token])
parseVariable (tok:rest) = Right (Grammar.Variable $ content tok, rest)

parseParen :: [Token] -> Either Error (Expression, [Token])
parseParen (parenToken:afterParen) = do
  (expression, afterExpression) <- parseExpression 0 afterParen
  let (tok:rest) = afterExpression
  let sym = symbol tok
  let closer = closingDelim $ symbol parenToken
  if | sym == closer -> return (expression, rest)
     | otherwise -> wrapError ExpectedSymbol {expected = [closer], actual = sym}

parsePrefixCall :: [Token] -> Either Error (Expression, [Token])
parsePrefixCall (tok:rest) = do
  (expression, afterExpression) <- parsePrefix rest
  return (Grammar.Call operatorStr [expression], afterExpression)
  where
    operatorStr = content tok

parseString :: [Token] -> Either Error (Expression, [Token])
parseString (_:contentToken:_:afterString) =
  Right (Grammar.String (content contentToken), afterString)
