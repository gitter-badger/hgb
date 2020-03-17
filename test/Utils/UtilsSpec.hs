module Utils.UtilsSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Utils

spec :: Spec
spec = do
  describe "isAnyOf" $ do
    it "returns false if the first argument is an empty list" $ do
      isAnyOf [] 8 `shouldBe` False
      isAnyOf [] "a" `shouldBe` False
    it "returns true if only one of the criteria matches" $ do
      isAnyOf [(== 2), (== 4)] 4 `shouldBe` True
    it "returns true if all criteria match" $ do
      isAnyOf [(== 4), (even), (/= 99)] 4 `shouldBe` True
  describe "SplitWhen" $ do
    it "splits a regular case correctly" $ do
      splitAtComma "a,b,see" `shouldBe` ["a", "b", "see"]
    it "splits and empty list correctly" $ do splitAtComma [] `shouldBe` [[]]
    it "splits correctly with adjacent delimiters" $ do
      splitAtComma "," `shouldBe` ["", ""]
      splitAtComma ",," `shouldBe` ["", "", ""]

splitAtComma :: String -> [String]
splitAtComma = splitWhen (== ',')
