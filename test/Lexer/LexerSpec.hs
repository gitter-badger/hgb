module Lexer.LexerSpec
  ( spec
  ) where

import Control.Monad
import Data.Function ((&))
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Lexer
import Symbol (Symbol(..))
import Token (Token(..))
import Utils (Span(..))

driveLexer :: String -> [String]
-- Removes end of file token
driveLexer s = init (lex s) & map show

spec :: Spec
spec = do
  describe "LexStr" $ do
    it "lexes strings correctly" $
      driveLexer "\"str\"" `shouldBe`
      ["StrBound(\")@0:1", "String(str)@1:4", "StrBound(\")@4:5"]
    describe "escape characters" $ do
      it "should work for string delims" $
        driveLexer "\"\\\"\"" `shouldBe`
        ["StrBound(\")@0:1", "String(\")@1:2", "StrBound(\")@2:3"]
      it "should work for newlines" $
        driveLexer "\"\\n\"" `shouldBe`
        ["StrBound(\")@0:1", "String(\n)@1:2", "StrBound(\")@2:3"]
      it "should work for tabs" $
        driveLexer "\"\\t\"" `shouldBe`
        ["StrBound(\")@0:1", "String(\t)@1:2", "StrBound(\")@2:3"]
      it "should not escape regular characters" $
        driveLexer "\"\\t\"" `shouldBe`
        ["StrBound(\")@0:1", "String(\t)@1:2", "StrBound(\")@2:3"]
      it "should not escape an empty escape sequence" $
        driveLexer "\"\\" `shouldBe` ["StrBound(\")@0:1", "String(\\)@1:2"]
    it "handles missing content" $
      driveLexer "\"\"" `shouldBe`
      ["StrBound(\")@0:1", "String()@1:1", "StrBound(\")@1:2"]
    it "handles missing ending quote" $
      driveLexer "\"str" `shouldBe` ["StrBound(\")@0:1", "String(str)@1:4"]
    it "lexes after string end" $
      driveLexer "\"str\" \"ing\"" `shouldBe`
      [ "StrBound(\")@0:1"
      , "String(str)@1:4"
      , "StrBound(\")@4:5"
      , "StrBound(\")@6:7"
      , "String(ing)@7:10"
      , "StrBound(\")@10:11"
      ]
  describe "LexNumber" $ do
    describe "lexes whole numbers" $ do
      it "lexes whole numbers" $ driveLexer "123" `shouldBe` ["Number(123)@0:3"]
      it "lexes whole numbers with zero prefix" $
        driveLexer "01" `shouldBe` ["Number(01)@0:2"]
      it "lexes after number end" $
        driveLexer "123 456" `shouldBe` ["Number(123)@0:3", "Number(456)@4:7"]
    describe "lexes floating point numbers" $ do
      it "lexes general floating point numbers" $
        driveLexer "123.456" `shouldBe` ["Number(123.456)@0:7"]
      it "does not lex prefixed decimal" $
        driveLexer ".456" `shouldBe` ["Dot(.)@0:1", "Number(456)@1:4"]
      it "lexes after number end" $
        driveLexer "12.34.56" `shouldBe`
        ["Number(12.34)@0:5", "Dot(.)@5:6", "Number(56)@6:8"]
  describe "LexKeyword" $
    it "lexes custom keywords correctly" $
    driveLexer "gingerbread" `shouldBe` ["Name(gingerbread)@0:11"]
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
      init (lex keyword) `shouldBe`
      [Token symbol keyword (Span 0 (length keyword))]
  describe "LexOperator" $ do
    it "lexes custom operators as invalid" $ do
      driveLexer "@1@4" `shouldBe`
        ["Invalid(@)@0:1", "Number(1)@1:2", "Invalid(@)@2:3", "Number(4)@3:4"]
      driveLexer "=/@" `shouldBe`
        ["Assign(=)@0:1", "Div(/)@1:2", "Invalid(@)@2:3"]
    it "does not lex combinations as invalid" $
      driveLexer "=/=!:" `shouldBe`
      ["NEq(=/=)@0:3", "LineDelim(!)@3:4", "TypeDelim(:)@4:5"]
  describe "lexAlphaKeyword" $ do
    it "lexes underscores in names correctly" $
      driveLexer "foo_bar" `shouldBe` ["Name(foo_bar)@0:7"]
    it "lexes numbers in names correctly" $
      driveLexer "foo22" `shouldBe` ["Name(foo22)@0:5"]
  describe "lex" $
    it "puts an EndOfFile Token at the end of the input" $ do
      let input = "foo_bar 1.5"
      let l = length input
      last (lex input) `shouldBe` Token Symbol.EndOfFile "<EOF>" (Span l l)
