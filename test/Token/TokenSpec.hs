module Token.TokenSpec
  ( spec
  ) where

import Test.Hspec

import Symbol (Symbol(..))
import Token (Token(..))

spec :: Spec
spec = do
  describe "Token" $ do
    it "should show correctly" $ do
      (show (Token Symbol.String "str")) `shouldBe` "Token String \"str\""
