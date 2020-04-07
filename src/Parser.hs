{-# LANGUAGE MultiWayIf #-}

module Parser
  ( parse
  ) where

import Data.Maybe
import Error (Error(..), ErrorType(..), Expectation(..))
import Grammar (Expression, Type(..))
import qualified Grammar
import Symbol (Symbol)
import qualified Symbol
import Token (Token(..))
import Utils

infixOperatorPrecidence :: Symbol -> Int
infixOperatorPrecidence op =
  case op of
    Symbol.Minus -> 1
    Symbol.Plus -> 1
    Symbol.Div -> 2
    Symbol.Times -> 2
    Symbol.Mod -> 2

closingDelim :: Symbol -> Symbol
closingDelim Symbol.LParen = Symbol.RParen

wrapExpectation :: Expectation -> Token -> Either Error any
wrapExpectation expectation gotTok =
  Left $ Error (Expected expectation gotTok) (sourceSpan gotTok)

type Delim = Symbol

type Terminator = Symbol

type ExpressionParser = ([Token] -> Either Error (Expression, [Token]))

type MultiExpressionParser = ([Token] -> Either Error ([Expression], [Token]))

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
-- Parses a program by parsing multiple lines of expressions until EOF is hit.
parse tokens = fst <$> parseLines Symbol.EndOfFile tokens

parseLines :: Terminator -> MultiExpressionParser
-- Parses multiple lines in program unitl some terminator is hit.
parseLines = parseMultiple (parseExpr 0 [Symbol.LineDelim])

parseMultiple :: ExpressionParser -> Terminator -> MultiExpressionParser
-- Parses multiple lines using an ExpressionParser some terminator is hit.
parseMultiple expressionParser terminator = parseMultiple'
  where
    parseMultiple' :: [Token] -> Either Error ([Expression], [Token])
    parseMultiple' tokens
      | symbol (head tokens) == terminator = Right ([], tokens)
      | otherwise = do
        (expr, afterExpr) <- expressionParser tokens
        -- An ExpressionParser should stop parsing right before a delimiter.
        let (delim:afterDelim) = afterExpr
        (otherExprs, beyond) <- parseMultiple' afterDelim
        return (expr : otherExprs, beyond)

parseExpr :: Int -> [Delim] -> ExpressionParser
parseExpr precedence delims tokens =
  if symbol (head tokens) == Symbol.Func
    then parseFunctionDeclaration $ tail tokens
    else do
      (left, afterLeft) <- parsePrefix delims tokens
      parseInfix precedence delims left afterLeft

parseFunctionDeclaration :: ExpressionParser
parseFunctionDeclaration tokens = do
  tokens <- expectedStart `assertMatches` tokens
  let nameTok:lParen:paramsAndBeyond = tokens
  (params, afterParams) <- parseDeclarations paramsAndBeyond
  tokens <- [Symbol.TypeDelim] `assertMatches` afterParams
  (functionType, afterType) <- parseType $ tail afterParams
  tokens <- [Symbol.LBrace] `assertMatches` afterType
  let lBrace:exprsAndBeyond = afterType
  (exprs, afterFunctionDeclaration) <- parseLines Symbol.RBrace exprsAndBeyond
  let name = content nameTok
  if null exprs
    then wrapExpectation Expression $ head exprsAndBeyond
    else return
           ( Grammar.FunctionDeclaration name params functionType exprs
           , afterFunctionDeclaration)
  where
    expectedStart = [Symbol.Name, Symbol.LParen]
    expectedMid = [Symbol.TypeDelim, Symbol.Type, Symbol.LBrace]
    parseDeclarations =
      parseEnumeration parseDeclare Symbol.ValueDelim Symbol.LParen
    assertMatches :: [Symbol] -> [Token] -> Either Error [Token]
    assertMatches symbols tokens =
      case returnConflict symbols tokens of
        Nothing -> Right tokens
        Just (expectedSymbol, actualToken) ->
          wrapExpectation (Symbol expectedSymbol) actualToken
    returnConflict :: [Symbol] -> [Token] -> Maybe (Symbol, Token)
    -- Check whether a list of matches a list of symbols at its start.
    -- If not, return a tuple of tokens that don't match the symbol.
    returnConflict [] _ = Nothing
    returnConflict (x:xs) (y:ys)
      | x == symbol y = returnConflict xs ys
      | otherwise = Just (x, y)

parsePrefix :: [Delim] -> ExpressionParser
-- Parse expressions that do not depend on previous parsing contexts.
parsePrefix delims tokens@(tok:_)
  | sym `elem` prefixOperators = parsePrefixCall delims tokens
  | sym == Symbol.LParen = parseParen tokens
  | sym == Symbol.Number = parseNumber tokens
  | sym == Symbol.Name = parseName delims tokens
  | sym == Symbol.StrBound = parseString tokens
  | sym == Symbol.Return = parseReturn tokens
  | otherwise = wrapExpectation Expression tok
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
parseInfix :: Int -> [Delim] -> Expression -> ExpressionParser
parseInfix precedence delims = parseInfix'
  where
    parseInfix' left tokens@(operatorTok:afterOperator)
      | operatorSym `elem` delims = Right (left, tokens)
      | operatorSym `elem` infixOperators =
        if newPrecidence <= precedence
          then Right (left, tokens)
          else do
            (right, afterExpr) <- parseExpr newPrecidence delims afterOperator
            let expr = Grammar.Call operatorStr [left, right]
            parseInfix' expr afterExpr
      | otherwise =
        wrapExpectation (Options $ Operator : map Symbol delims) operatorTok
      where
        infixOperators =
          [Symbol.Plus, Symbol.Minus, Symbol.Times, Symbol.Div, Symbol.Mod]
        operatorSym = symbol operatorTok
        newPrecidence = infixOperatorPrecidence operatorSym
        operatorStr = content operatorTok

parseNumber :: ExpressionParser
parseNumber (tok:rest) = Right (Grammar.Number $ content tok, rest)

parseName :: [Delim] -> ExpressionParser
parseName delims tokens@(nameTok:next:beyond) =
  case symbol next of
    Symbol.LParen -> parseCall tokens
    Symbol.TypeDelim -> parseDeclare delims tokens
    Symbol.Assign -> parseAssign delims tokens
    _ -> parseReference tokens

parseCall :: ExpressionParser
-- Parses a call expression of the form
--   foo(data, "value")!
--   ^^^                  | name
--      ^                 | lParen
--       ^^^^^^^^^^^^^^^  | afterLParen
parseCall (nameTok:lParen:afterLParen) = do
  (arguments, afterArguments) <- parseEnumeration' Symbol.LParen afterLParen
  return (Grammar.Call name arguments, afterArguments)
  where
    name = content nameTok
    parseFunction = parseExpr 0
    parseEnumeration' = parseEnumeration parseFunction Symbol.ValueDelim

parseDeclare :: [Delim] -> ExpressionParser
parseDeclare delims (nameTok:typeDelim:afterTypeDelim) = do
  (varType, afterType) <- parseType afterTypeDelim
  let (next:beyond) = afterType
  if | symbol next == Symbol.Assign ->
       do (declaration, afterDeclaration) <- parseExpr 0 delims beyond
          return
            ( Grammar.Declaration name varType (Just declaration)
            , afterDeclaration)
     | symbol next `elem` delims ->
       return (Grammar.Declaration name varType Nothing, afterType)
     | otherwise ->
       wrapExpectation (Options $ Assignment : map Symbol delims) next
  where
    name = content nameTok

parseAssign :: [Delim] -> ExpressionParser
parseAssign delims (nameTok:assign:afterAssign) = do
  (assignedVal, afterVal) <- parseExpr 0 delims afterAssign
  return (Grammar.Assignment (content nameTok) assignedVal, afterVal)

parseEnumeration ::
     ([Delim] -> ExpressionParser)
  -> Delim
  -> Terminator
  -> MultiExpressionParser
-- Parses an enumerated list
--   (data, "value")!
--   ^                 | list open
--                 ^   | list close
--    ^^^^^^^^^^^^^^^  | afterOpen
-- The difference between parseEnumeration and parseMultiple is that parseEnum
-- does not require an enumerator at the end of the last expression whereas
-- parseMultiple does. For example
--     parseEnumeration parseExpr Symbol.ValueDelim Symbol.LParen
-- would parse enumerations like "(a, b, c)", whereas
--     parseMultiple (parseExpr 0 Symbol.ValueDelim) Symbol.LParen
--  would parse things like "(a, b, c, )" -- notice the trailing comma.
parseEnumeration vagueExpressionParser delim listOpen afterOpen
  | symbol (head afterOpen) == listClose = Right ([], tail afterOpen)
  | otherwise = parseEnumeration' afterOpen
  where
    listClose = closingDelim listOpen
    parseFunction = vagueExpressionParser [delim, listClose]
    parseEnumeration' :: [Token] -> Either Error ([Expression], [Token])
    -- Parse tokens after the enumerated item. There are two cases here:
    --   1. the item is followed by a item delim
    --   2. the item is followed by a list close
    parseEnumeration' tokens = do
      (item, afterItem) <- parseFunction tokens
      let (itemClose:restOfListAndBeyond) = afterItem
      let sym = symbol itemClose
      if | sym == Symbol.ValueDelim ->
           do (otherItems, beyondList) <- parseEnumeration' restOfListAndBeyond
              return (item : otherItems, beyondList)
         | sym == listClose -> return ([item], restOfListAndBeyond)

parseReference :: ExpressionParser
parseReference (nameTok:rest) = Right (Grammar.Reference name, rest)
  where
    name = content nameTok

parseParen :: ExpressionParser
parseParen (parenToken:afterParen) = do
  (expr, afterExpr) <- parseExpr 0 [Symbol.RParen] afterParen
  let closer:rest = afterExpr
  return (expr, rest)

parsePrefixCall :: [Delim] -> ExpressionParser
parsePrefixCall delims (tok:rest) = do
  (expr, afterExpr) <- parsePrefix delims rest
  return (Grammar.Call (content tok) [expr], afterExpr)

parseString :: ExpressionParser
parseString (_:contentToken:_:afterString) =
  Right (Grammar.String (content contentToken), afterString)

parseReturn :: ExpressionParser
parseReturn (returnTok:afterKeyword) = do
  (expr, afterExpr) <- parseExpr 0 [Symbol.LineDelim] afterKeyword
  return (Grammar.Return expr, afterExpr)

parseType :: [Token] -> Either Error (Grammar.Type, [Token])
parseType (tok:afterTok) =
  case symbol tok of
    Symbol.Type -> Right (read $ content tok, afterTok)
    Symbol.LBracket -> do
      (arrayType, afterType) <- parseType afterTok
      if symbol (head afterType) == Symbol.RBracket
        then return (Array arrayType, tail afterType)
        else wrapExpectation (Symbol Symbol.RBracket) (head afterType)
    _ -> wrapExpectation TypeDeclaration tok
