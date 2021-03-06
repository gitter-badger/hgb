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
  describe "Expectations" $
    forM_
      [ ("expression", Expression, "an expression")
      , ("operator", Operator, "an operator")
      , ("a symbol", Symbol Symbol.ValueDelim, "\",\"")
      ] $ \(name, expectation, str) ->
      it ("should show expectations: " ++ name) $
      show expectation `shouldBe` str
  describe "Error" $
    forM_
      [ ( "expected"
        , Expected [Expression] (Token Symbol.LineDelim "!" (Span 0 1))
        , "expected an expression, got \"!\"")
      ] $ \(name, err, str) ->
      it ("should show error types: " ++ name) $ show err `shouldBe` str
  describe "Error" $
    it "should show errors" $ do
      let span = Span 0 1
      let expected = Expected [Expression] (Token Symbol.LineDelim "!" span)
      show (Error expected span) `shouldBe`
        "Error: expected an expression, got \"!\" at 0:1"
