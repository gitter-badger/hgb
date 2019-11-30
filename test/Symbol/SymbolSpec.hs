module Symbol.SymbolSpec
  ( spec
  ) where

import Symbol (Symbol(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Symbol" $ do
    it "should show correctly" $ do
      (show Symbol.String) `shouldBe` "String"
