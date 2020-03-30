module Eval.EvalSpec
  ( spec
  ) where

import Eval
import Test.Hspec

import Lexer (lex)
import Parser (parse)
import Prelude hiding (lex)

driveEvalInt :: String -> Maybe Int
driveEvalInt = either (const . Nothing) (Just . head) . parse . lex
  where
    unpackRight (Left a) = Nothing
    unpackRight (Right a) = a

generateExpectation name input expected =
  it (name ++ " like " ++ input) $ driveEvalInt `shouldBe` Just expected

spec :: Spec
spec = do
  describe "Eval" $
    it "should evaluate numbers no problem" $
    driveEvalInt "1!" `shouldBe`
    Just
      1
      map
      (generateExpectation "should evaluate simple expressions")
      [ ("1 + 1!", 2)
      , ("4 * 5!", 20)
      , ("20 % 7!", 6)
      , ("10 - 8!", 2)
      , ("10 / 4!", 2)
      , ("-5!", -5)
      , ("+5!", 5)
      ]
      map
      (generateExpectation "should evaluate bracketed expressions")
      [ ("1 + (2 + 3)!", 6)
      , ("1 - (2 + 3)!", -4)
      , ("1 * (2 + 3)!", 5)
      , ("1 / (2 + 3)!", 0)
      , ("1 % (2 + 3)!", 1)
      , ("1 + (2 + (3 + 4))!", 10)
      ] $ \(str, result) ->
      map
        generateExpectation
        "should evaluate expressions with differing precidence"
        [ ("1 + 2 * 3!", 7)
        , ("1 * 2 + 3!", 5)
        , ("1 - 2 * 3!", 5)
        , ("1 * 2 - 3!", -1)
        , ("1 + 2 / 3!", 1)
        , ("1 / 2 + 3!", 3)
        , ("1 - 2 / 3!", 1)
        , ("1 / 2 - 3!", -3)
        , ("1 + 2 % 3!", 3)
        , ("1 % 2 + 3!", 4)
        , ("1 - 2 % 3!", -1)
        , ("1 % 2 - 3!", -2)
        ]
