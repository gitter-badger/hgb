module Lexer.LexerSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Text.Printf

import qualified Builtin
import Data (Token(..), hgbLex)
import Symbol (Symbol(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "LexStr" $ do
    it "lexes strings correctly" $ do
      hgbLex "\"str\"" `shouldBe`
        [ (Token Symbol.StrBound "\"")
        , (Token Symbol.String "str")
        , (Token Symbol.StrBound "\"")
        ]
    it "handles missing content" $ do
      hgbLex "\"\"" `shouldBe`
        [(Token Symbol.StrBound "\""), (Token Symbol.StrBound "\"")]
    it "handles missing ending quote" $ do
      hgbLex "\"str" `shouldBe`
        [(Token Symbol.StrBound "\""), (Token Symbol.String "str")]
    it "lexes after string end" $ do
      hgbLex "\"str\" \"ing\"" `shouldBe`
        [ (Token Symbol.StrBound "\"")
        , (Token Symbol.String "str")
        , (Token Symbol.StrBound "\"")
        , (Token Symbol.StrBound "\"")
        , (Token Symbol.String "ing")
        , (Token Symbol.StrBound "\"")
        ]
  describe "LexNumber" $ do
    it "lexes numbers correctly" $ do
      hgbLex "123" `shouldBe` [(Token Symbol.Number "123")]
    it "lexes after number end" $ do
      hgbLex "123 456" `shouldBe`
        [(Token Symbol.Number "123"), (Token Symbol.Number "456")]
  describe "LexKeyword" $ do
    it "lexes custom keywords correctly" $ do
      hgbLex "gingerbread" `shouldBe` [(Token Symbol.Name "gingerbread")]
  describe "Lex Builtin" $ do
    let cases =
          [ (Builtin.KeywordIterUpTo, Symbol.IterUpTo)
          , (Builtin.KeywordNEq, Symbol.NEq)
          , (Builtin.KeywordGEq, Symbol.GEq)
          , (Builtin.KeywordLEq, Symbol.LEq)
          , (Builtin.KeywordEq, Symbol.Eq)
          , (Builtin.KeywordDot, Symbol.Dot)
          , (Builtin.KeywordLBracket, Symbol.LBracket)
          , (Builtin.KeywordRBracket, Symbol.RBracket)
          , (Builtin.KeywordLParen, Symbol.LParen)
          , (Builtin.KeywordRParen, Symbol.RParen)
          , (Builtin.KeywordLBrace, Symbol.LBrace)
          , (Builtin.KeywordRBrace, Symbol.RBrace)
          , (Builtin.KeywordTypeDelim, Symbol.TypeDelim)
          , (Builtin.KeywordValueDelim, Symbol.ValueDelim)
          , (Builtin.KeywordAssign, Symbol.Assign)
          , (Builtin.KeywordGT, Symbol.GT)
          , (Builtin.KeywordLT, Symbol.LT)
          , (Builtin.KeywordPlus, Symbol.Plus)
          , (Builtin.KeywordMinus, Symbol.Minus)
          , (Builtin.KeywordTimes, Symbol.Times)
          , (Builtin.KeywordDiv, Symbol.Div)
          , (Builtin.KeywordMod, Symbol.Mod)
          , (Builtin.KeywordCharBound, Symbol.CharBound)
          , (Builtin.KeywordStrBound, Symbol.StrBound)
          , (Builtin.KeywordExprEnd, Symbol.ExprEnd)
          , (Builtin.KeywordIn, Symbol.In)
          , (Builtin.KeywordAnd, Symbol.And)
          , (Builtin.KeywordNot, Symbol.Not)
          , (Builtin.KeywordOr, Symbol.Or)
          , (Builtin.KeywordFor, Symbol.For)
          , (Builtin.KeywordWhile, Symbol.While)
          , (Builtin.KeywordIf, Symbol.If)
          , (Builtin.KeywordElse, Symbol.Else)
          , (Builtin.KeywordElseIf, Symbol.ElseIf)
          , (Builtin.KeywordFunc, Symbol.Func)
          , (Builtin.KeywordReturn, Symbol.Return)
          , (Builtin.KeywordInt8, Symbol.Type)
          , (Builtin.KeywordInt16, Symbol.Type)
          , (Builtin.KeywordInt32, Symbol.Type)
          , (Builtin.KeywordInt64, Symbol.Type)
          , (Builtin.KeywordInt, Symbol.Type)
          , (Builtin.KeywordUInt8, Symbol.Type)
          , (Builtin.KeywordUInt16, Symbol.Type)
          , (Builtin.KeywordUInt32, Symbol.Type)
          , (Builtin.KeywordUInt64, Symbol.Type)
          , (Builtin.KeywordUInt, Symbol.Type)
          , (Builtin.KeywordChar, Symbol.Type)
          , (Builtin.KeywordVoid, Symbol.Type)
          ]
    forM_ cases $ \(keyword, symbol) -> do
      it ("lexes " ++ (show symbol) ++ ": " ++ keyword) $ do
        hgbLex keyword `shouldBe` [(Token symbol keyword)]
  describe "LexOperator" $ do
    it "lexes custom operators as invalid" $ do
      hgbLex "@^@" `shouldBe` [(Token Symbol.Invalid "@^@")]
