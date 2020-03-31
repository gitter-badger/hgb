module Eval.EvalSpec
  ( spec
  ) where

import Control.Monad
import Eval
import Test.Hspec

import Error (Error)
import Grammar (Expression)
import Lexer (lex)
import Parser (parse)
import Prelude hiding (lex)

driveEvalInt :: String -> Either String Int
driveEvalInt s =
  case parse $ lex s of
    Left err -> Left $ show err
    Right (expr:exprs) -> Right $ evalInt expr

generateSpec name (input, expected) =
  it (name ++ " like " ++ input) $ driveEvalInt input `shouldBe` Right expected

spec :: Spec
spec =
  describe "Eval" $
  it "should evaluate numbers no problem" $
  driveEvalInt "1!" `shouldBe`
  Right
    1
    forM_
    [ ("1 + 1!", 2)
    , ("4 * 5!", 20)
    , ("20 % 7!", 6)
    , ("10 - 8!", 2)
    , ("10 / 4!", 2)
    , ("-5!", -5)
    , ("+5!", 5)
    ] $
  generateSpec
    "should evaluate simple expressions"
    forM_
    [ ("1 + (2 + 3)!", 6)
    , ("1 - (2 + 3)!", -4)
    , ("1 * (2 + 3)!", 5)
    , ("1 / (2 + 3)!", 0)
    , ("1 % (2 + 3)!", 1)
    , ("1 + (2 + (3 + 4))!", 10)
    ] $
  generateSpec
    "should evaluate bracketed expressions"
    forM_
    [ ("1 + 2 * 3!", 7)
    , ("1 * 2 + 3!", 5)
    , ("1 - 2 * 3!", -5)
    , ("1 * 2 - 3!", -1)
    , ("1 + 2 / 3!", 1)
    , ("1 / 2 + 3!", 3)
    , ("1 - 2 / 3!", 1)
    , ("1 / 2 - 3!", -3)
    , ("1 + 2 % 3!", 3)
    , ("1 % 2 + 3!", 4)
    , ("1 - 2 % 3!", -1)
    , ("1 % 2 - 3!", -2)
    ] $
  generateSpec "should evaluate expressions with differing precidence"
