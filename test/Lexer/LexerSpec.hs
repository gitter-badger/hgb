module Lexer.LexerSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Lexer (lex)
import Symbol (Symbol(..))
import Token (Token(..))

spec :: Spec
spec = do
  describe "LexStr" $ do
    it "lexes strings correctly" $ do
      lex "\"str\"" `shouldBe`
        [ (Token Symbol.StrBound "\"" 0 1)
        , (Token Symbol.String "str" 1 4)
        , (Token Symbol.StrBound "\"" 4 5)
        ]
    it "handles missing content" $ do
      lex "\"\"" `shouldBe`
        [ (Token Symbol.StrBound "\"" 0 1)
        , (Token Symbol.String "" 1 1)
        , (Token Symbol.StrBound "\"" 1 2)
        ]
    it "handles missing ending quote" $ do
      lex "\"str" `shouldBe`
        [(Token Symbol.StrBound "\"" 0 1), (Token Symbol.String "str" 1 4)]
    it "lexes after string end" $ do
      lex "\"str\" \"ing\"" `shouldBe`
        [ (Token Symbol.StrBound "\"" 0 1)
        , (Token Symbol.String "str" 1 4)
        , (Token Symbol.StrBound "\"" 4 5)
        , (Token Symbol.StrBound "\"" 6 7)
        , (Token Symbol.String "ing" 7 10)
        , (Token Symbol.StrBound "\"" 10 11)
        ]
  describe "LexNumber" $ do
    describe "lexes whole numbers" $ do
      it "lexes general whole numbers" $ do
        lex "123" `shouldBe` [(Token Symbol.Number "123" 0 3)]
      it "lexes after number end" $ do
        lex "123 456" `shouldBe`
          [(Token Symbol.Number "123" 0 3), (Token Symbol.Number "456" 4 7)]
    describe "lexes floating point numbers" $ do
      it "lexes general floating point numbers" $ do
        lex "123.456" `shouldBe` [(Token Symbol.Number "123.456" 0 7)]
      it "does not lex prefixed decimal" $ do
        lex ".456" `shouldBe`
          [(Token Symbol.Dot "." 0 1), (Token Symbol.Number "456" 1 4)]
      it "lexes after number end" $ do
        lex "12.34.56" `shouldBe`
          [ (Token Symbol.Number "12.34" 0 5)
          , (Token Symbol.Dot "." 5 6)
          , (Token Symbol.Number "56" 6 8)
          ]
  describe "LexKeyword" $ do
    it "lexes custom keywords correctly" $ do
      lex "gingerbread" `shouldBe` [(Token Symbol.Name "gingerbread" 0 11)]
  describe "Lex Builtin" $ do
    let cases =
          [ ("->", Symbol.IterUpTo)
          , ("=/=", Symbol.NEq)
          , (">=", Symbol.GEq)
          , ("<=", Symbol.LEq)
          , ("==", Symbol.Eq)
          , (".", Symbol.Dot)
          , ("[", Symbol.LBracket)
          , ("]", Symbol.RBracket)
          , ("(", Symbol.LParen)
          , (")", Symbol.RParen)
          , ("{", Symbol.LBrace)
          , ("}", Symbol.RBrace)
          , (":", Symbol.TypeDelim)
          , (",", Symbol.ValueDelim)
          , ("=", Symbol.Assign)
          , (">", Symbol.GT)
          , ("<", Symbol.LT)
          , ("+", Symbol.Plus)
          , ("-", Symbol.Minus)
          , ("*", Symbol.Times)
          , ("/", Symbol.Div)
          , ("%", Symbol.Mod)
          , ("!", Symbol.ExprEnd)
          , ("in", Symbol.In)
          , ("and", Symbol.And)
          , ("not", Symbol.Not)
          , ("or", Symbol.Or)
          , ("for", Symbol.For)
          , ("while", Symbol.While)
          , ("if", Symbol.If)
          , ("else", Symbol.Else)
          , ("elseif", Symbol.ElseIf)
          , ("fun", Symbol.Func)
          , ("return", Symbol.Return)
          , ("int8", Symbol.Type)
          , ("int16", Symbol.Type)
          , ("int32", Symbol.Type)
          , ("int64", Symbol.Type)
          , ("int", Symbol.Type)
          , ("uint8", Symbol.Type)
          , ("uint16", Symbol.Type)
          , ("uint32", Symbol.Type)
          , ("uint64", Symbol.Type)
          , ("uint", Symbol.Type)
          , ("double", Symbol.Type)
          , ("void", Symbol.Type)
          ]
    forM_ cases $ \(keyword, symbol) -> do
      it ("lexes " ++ (show symbol) ++ ": " ++ keyword) $ do
        lex keyword `shouldBe` [(Token symbol keyword 0 (length keyword))]
  describe "LexOperator" $ do
    it "lexes custom operators as invalid" $ do
      lex "@^@" `shouldBe`
        [ (Token Symbol.Invalid "@" 0 1)
        , (Token Symbol.Invalid "^" 1 2)
        , (Token Symbol.Invalid "@" 2 3)
        ]
    it "does not lex combinations as invalid" $ do
      lex "=/=!:" `shouldBe`
        [ (Token Symbol.NEq "=/=" 0 3)
        , (Token Symbol.ExprEnd "!" 3 4)
        , (Token Symbol.TypeDelim ":" 4 5)
        ]
  describe "lexAlphaKeyword" $ do
    it "lexes underscores in names correctly" $ do
      lex "foo_bar" `shouldBe` [(Token Symbol.Name "foo_bar" 0 7)]
    it "lexes numbers in names correctly" $ do
      lex "foo22" `shouldBe` [(Token Symbol.Name "foo22" 0 5)]
