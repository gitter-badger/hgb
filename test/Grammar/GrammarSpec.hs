module Grammar.GrammarSpec
  ( spec
  ) where

import Control.Monad
import Data.Semigroup
import Prelude hiding (lex)
import Text.Printf

import Test.Hspec

import Grammar

types =
  [ (Double, "double")
  , (Void, "void")
  , (Int Nothing True, "int")
  , (Int (Just B8) True, "int8")
  , (Int (Just B16) True, "int16")
  , (Int (Just B32) True, "int32")
  , (Int (Just B64) True, "int64")
  , (Int Nothing False, "uint")
  , (Int (Just B8) False, "uint8")
  , (Int (Just B16) False, "uint16")
  , (Int (Just B32) False, "uint32")
  , (Int (Just B64) False, "uint64")
  ]

nestType :: (Type, String) -> [(Expression, String)]
nestType (ty, tyStr) =
  [ (Declaration "var" ty Nothing, "(declare ($var : " ++ tyStr ++ "))")
  , ( Declaration "var" ty (Just $ Number 1)
    , "(declare ($var : " ++ tyStr ++ ") 1)")
  , ( FunctionDeclaration "func" [] ty []
    , "(fun (func () : " ++ tyStr ++ ") ())")
  ]

baseExpressions =
  [ (Number 5, "5")
  , (String "str", "\"str\"")
  , (Reference "data", "$data")
  , (Call "func" [], "(func )")
  ] ++
  concat (map nestType types)

nestExpression :: (Expression, String) -> [(Expression, String)]
nestExpression (expr, exprStr) =
  [ ( FunctionDeclaration "func" [] Void [expr]
    , "(fun (func () : void) (" ++ exprStr ++ "))")
  , (Call "func" [expr], "(func " ++ exprStr ++ ")")
  , (Return expr, "(return " ++ exprStr ++ ")")
  ]

baseAndNestedExpressions =
  baseExpressions ++ concat (map nestExpression baseExpressions)

spec :: Spec
spec = do
  describe "Type" $ do
    it "should access int fields" $ do
      let n = Int (Just B16) True
      numBits n `shouldBe` Just B16
      signed n `shouldBe` True
    forM_ types $ \(ty, str) -> do
      it ("should show the type " ++ str) $ show ty `shouldBe` str
      it ("should read the type " ++ str) $ read str `shouldBe` ty
  describe "Expression" $ do
    forM_ baseAndNestedExpressions $ \(sym, str) ->
      it ("should show expression: " ++ str) $ show sym `shouldBe` str
    it "should access call fields" $ do
      let op = Call "func" [Number 0, Reference "str", String "str"]
      (name op, arguments op) `shouldBe`
        ("func", [Number 0, Reference "str", String "str"])
    it "should access declaration fields" $ do
      let d = Declaration "a" (Int Nothing True) Nothing
      name d `shouldBe` "a"
      ty d `shouldBe` Int Nothing True
      maybeAssignment d `shouldBe` Nothing
    it "should access assignment fields" $ do
      let a = Assignment "a" $ Number 5
      name a `shouldBe` "a"
      assignment a `shouldBe` Number 5
    it "should access function declaration Fields" $ do
      let f = FunctionDeclaration "foo" [] Void [Return $ Number 1]
      name f `shouldBe` "foo"
      params f `shouldBe` []
      ty f `shouldBe` Void
      exprs f `shouldBe` [Return $ Number 1]
