module Symbol.SymbolSpec
  ( spec
  ) where

import Test.Hspec

import Symbol (Symbol(..))

spec :: Spec
spec = do
  describe "Symbol" $ do
    it "should show correctly" $ do (show Symbol.String) `shouldBe` "String"
