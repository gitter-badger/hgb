module Lexer.LexerSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Lexer
import Symbol (Symbol(..))
import Token (Token(..))
import Utils (Span(..))

driveLexer :: String -> [Token]
-- Removes end of file token
driveLexer s = init (lex s)

spec :: Spec
spec = do
  describe "LexStr" $ do
    it "lexes strings correctly" $
      driveLexer "\"str\"" `shouldBe`
      [ Token Symbol.StrBound "\"" (Span 0 1)
      , Token Symbol.String "str" (Span 1 4)
      , Token Symbol.StrBound "\"" (Span 4 5)
      ]
    describe "escape characters" $ do
      it "should work for string delims" $
        driveLexer "\"\\\"\"" `shouldBe`
        [ Token Symbol.StrBound "\"" (Span 0 1)
        , Token Symbol.String "\"" (Span 1 2)
        , Token Symbol.StrBound "\"" (Span 2 3)
        ]
      it "should work for newlines" $
        driveLexer "\"\\n\"" `shouldBe`
        [ Token Symbol.StrBound "\"" (Span 0 1)
        , Token Symbol.String "\n" (Span 1 2)
        , Token Symbol.StrBound "\"" (Span 2 3)
        ]
      it "should work for tabs" $
        driveLexer "\"\\t\"" `shouldBe`
        [ Token Symbol.StrBound "\"" (Span 0 1)
        , Token Symbol.String "\t" (Span 1 2)
        , Token Symbol.StrBound "\"" (Span 2 3)
        ]
      it "should not escape regular characters" $
        driveLexer "\"\\t\"" `shouldBe`
        [ Token Symbol.StrBound "\"" (Span 0 1)
        , Token Symbol.String "\t" (Span 1 2)
        , Token Symbol.StrBound "\"" (Span 2 3)
        ]
      it "should not escape an empty escape sequence" $
        driveLexer "\"\\" `shouldBe`
        [ Token Symbol.StrBound "\"" (Span 0 1)
        , Token Symbol.String "\\" (Span 1 2)
        ]
    it "handles missing content" $
      driveLexer "\"\"" `shouldBe`
      [ Token Symbol.StrBound "\"" (Span 0 1)
      , Token Symbol.String "" (Span 1 1)
      , Token Symbol.StrBound "\"" (Span 1 2)
      ]
    it "handles missing ending quote" $
      driveLexer "\"str" `shouldBe`
      [ Token Symbol.StrBound "\"" (Span 0 1)
      , Token Symbol.String "str" (Span 1 4)
      ]
    it "lexes after string end" $
      driveLexer "\"str\" \"ing\"" `shouldBe`
      [ Token Symbol.StrBound "\"" (Span 0 1)
      , Token Symbol.String "str" (Span 1 4)
      , Token Symbol.StrBound "\"" (Span 4 5)
      , Token Symbol.StrBound "\"" (Span 6 7)
      , Token Symbol.String "ing" (Span 7 10)
      , Token Symbol.StrBound "\"" (Span 10 11)
      ]
  describe "LexNumber" $ do
    describe "lexes whole numbers" $ do
      it "lexes whole numbers" $
        driveLexer "123" `shouldBe` [Token Symbol.Number "123" (Span 0 3)]
      it "lexes whole numbers with zero prefix" $
        driveLexer "01" `shouldBe` [Token Symbol.Number "01" (Span 0 2)]
      it "lexes after number end" $
        driveLexer "123 456" `shouldBe`
        [ Token Symbol.Number "123" (Span 0 3)
        , Token Symbol.Number "456" (Span 4 7)
        ]
    describe "lexes floating point numbers" $ do
      it "lexes general floating point numbers" $
        driveLexer "123.456" `shouldBe`
        [Token Symbol.Number "123.456" (Span 0 7)]
      it "does not lex prefixed decimal" $
        driveLexer ".456" `shouldBe`
        [Token Symbol.Dot "." (Span 0 1), Token Symbol.Number "456" (Span 1 4)]
      it "lexes after number end" $
        driveLexer "12.34.56" `shouldBe`
        [ Token Symbol.Number "12.34" (Span 0 5)
        , Token Symbol.Dot "." (Span 5 6)
        , Token Symbol.Number "56" (Span 6 8)
        ]
  describe "LexKeyword" $
    it "lexes custom keywords correctly" $
    driveLexer "gingerbread" `shouldBe`
    [Token Symbol.Name "gingerbread" (Span 0 11)]
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
          , ("!", Symbol.LineDelim)
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
    forM_ cases $ \(keyword, symbol) ->
      it ("lexes " ++ show symbol ++ ": " ++ keyword) $
      driveLexer keyword `shouldBe`
      [Token symbol keyword (Span 0 (length keyword))]
  describe "LexOperator" $ do
    it "lexes custom operators as invalid" $ do
      driveLexer "@1@4" `shouldBe`
        [ Token Symbol.Invalid "@" (Span 0 1)
        , Token Symbol.Number "1" (Span 1 2)
        , Token Symbol.Invalid "@" (Span 2 3)
        , Token Symbol.Number "4" (Span 3 4)
        ]
      driveLexer "=/@" `shouldBe`
        [ Token Symbol.Assign "=" (Span 0 1)
        , Token Symbol.Div "/" (Span 1 2)
        , Token Symbol.Invalid "@" (Span 2 3)
        ]
    it "does not lex combinations as invalid" $
      driveLexer "=/=!:" `shouldBe`
      [ Token Symbol.NEq "=/=" (Span 0 3)
      , Token Symbol.LineDelim "!" (Span 3 4)
      , Token Symbol.TypeDelim ":" (Span 4 5)
      ]
  describe "lexAlphaKeyword" $ do
    it "lexes underscores in names correctly" $
      driveLexer "foo_bar" `shouldBe` [Token Symbol.Name "foo_bar" (Span 0 7)]
    it "lexes numbers in names correctly" $
      driveLexer "foo22" `shouldBe` [Token Symbol.Name "foo22" (Span 0 5)]
  describe "lex" $
    it "puts an EndOfFile Token at the end of the input" $ do
      let input = "foo_bar 1.5"
      let l = length input
      last (lex input) `shouldBe` Token Symbol.EndOfFile "<EOF>" (Span l l)
