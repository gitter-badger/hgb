module Token.TokenSpec
  ( spec
  ) where

import Data (Token(..))
import Symbol (Symbol(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Token" $ do
    it "should show correctly" $ do
      (show (Token Symbol.String "str")) `shouldBe` "Token String \"str\""
