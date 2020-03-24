module Error.ErrorSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Symbol (Symbol(..))
import Test.Hspec
import Token (Token(..))
import Utils (Span(..))

import Error

spec :: Spec
spec = do
  describe "Expectation" $
    forM_
      [ ("expression", Expression, "an expression")
      , ("expression terminator", ExpressionTerminator, "\"!\"")
      , ( "expression or delimiter"
        , ExpressionOrDelimiter
        , "an expression or delimiter")
      , ( "enumeration list delimiter: one option"
        , EnumerationListDelimiter [Symbol.ValueDelim]
        , "\",\"")
      , ( "enumeration list delimiter: two options"
        , EnumerationListDelimiter [Symbol.ValueDelim, Symbol.RParen]
        , "\",\" or \")\"")
      , ( "enumeration list delimiter: multiple options"
        , EnumerationListDelimiter
            [Symbol.ValueDelim, Symbol.RParen, Symbol.ExprEnd]
        , "\",\", \")\", or \"!\"")
      , ("close token", CloseToken Symbol.RParen, "\")\"")
      ] $ \(name, err, str) ->
      it ("should show expectations: " ++ name) $ show err `shouldBe` str
  describe "ErrorType" $
    forM_
      [ ( "expected"
        , Expected Expression (Token Symbol.ExprEnd "!" (Span 0 1))
        , "expected an expression, got \"!\"")
      ] $ \(name, err, str) ->
      it ("should show error types: " ++ name) $ show err `shouldBe` str
  describe "Error" $
    it "should show errors" $ do
      let span = Span 0 1
      let expected = Expected Expression (Token Symbol.ExprEnd "!" span)
      show (Error expected span) `shouldBe`
        "Error: expected an expression, got \"!\" at 0:1"
