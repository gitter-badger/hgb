module Grammar.GrammarSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Grammar

spec :: Spec
spec =
  describe "Expression" $ do
    forM_
      [ (Number 5, "5")
      , (String "str", "\"str\"")
      , (Variable "data", "$data")
      , (Call "func" [], "(func )")
      , (Call "func" [Number 0], "(func 0)")
      , (Call "func" [String "str"], "(func \"str\")")
      , (Call "func" [Variable "data"], "(func $data)")
      , (Call "func" [Call "f" []], "(func (f ))")
      , (Call "func" [Number 0, Number 1], "(func 0 1)")
      , (Call "" [Number 0, Number 1], "( 0 1)")
      ] $ \(sym, str) -> it "should show datatypes" $ show sym `shouldBe` str
    it "should access operation fields" $ do
      let op = Call "func" [Number 0, Variable "str", String "str"]
      (name op, arguments op) `shouldBe`
        ("func", [Number 0, Variable "str", String "str"])
