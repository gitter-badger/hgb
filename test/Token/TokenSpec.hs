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
      (show (Token Symbol.String "str" 0 3)) `shouldBe` "Token {symbol = String, content = \"str\", start = 0, end = 3}"
    it "should access each data field correctly" $ do
      let t = Token Symbol.Name "zifah" 5 10
      (symbol t, content t, start t, end t) `shouldBe` (Symbol.Name, "zifah", 5, 10)
