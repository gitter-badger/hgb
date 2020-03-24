module Parser.ParserSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Data.Function ((&))
import Prelude hiding (lex)
import Test.Hspec

import Lexer
import Parser

-- Parses a Gingerbread program into Shown expressions or errors.
-- See the Show instances in src/Grammar.hs and src/Error.hs for expected formats.
driveParser :: String -> String
driveParser s = lex s & parse & either show (unwords . map show)

spec :: Spec
spec = do
  describe "numbers" $
    it "should parse numbers" $ driveParser "3!" `shouldBe` "3"
  describe "strings" $
    it "should parse strings" $ driveParser "\"foo\"!" `shouldBe` "\"foo\""
  describe "variables" $
    it "should parse variables" $ driveParser "hey!" `shouldBe` "$hey"
  describe "calls" $ do
    forM_
      [ ("without arguments", "foo()!", "(foo )")
      , ("with one argument", "foo(1)!", "(foo 1)")
      , ( "with multiple arguments"
        , "foo(1, data, \"str\")!"
        , "(foo 1 $data \"str\")")
      , ("with multiple arguments", "foo(f())!", "(foo (f ))")
      ] $ \(name, str, shown) ->
      it ("should parse calls: " ++ name) $ driveParser str `shouldBe` shown
    describe "prefix sign calls" $ do
      it "should parse positive calls" $ driveParser "+2!" `shouldBe` "(+ 2)"
      it "should parse negative calls" $ driveParser "-2!" `shouldBe` "(- 2)"
      it "should parse chained infix calls" $
        driveParser "-+-2!" `shouldBe` "(- (+ (- 2)))"
    describe "infix calls" $
      forM_
        [ ("addition", "5 + 2!", "(+ 5 2)")
        , ("subtraction", "5 - 2!", "(- 5 2)")
        , ("multiplication", "5 * 2!", "(* 5 2)")
        , ("division", "5 / 2!", "(/ 5 2)")
        , ("modulo", "5 % 2!", "(% 5 2)")
        ] $ \(name, str, shown) ->
        it ("should parse " ++ name) $ driveParser str `shouldBe` shown
  describe "parse precendence" $ do
    forM_
      -- Flat precedence
      [ ("+, - calls", "1 + 2 - 3!", "(- (+ 1 2) 3)")
      , ("*, /, % calls", "1 * 2 / 3 % 4!", "(% (/ (* 1 2) 3) 4)")
      ] $ \(name, str, shown) ->
      it ("should parse tokens of the same precedence: " ++ name) $
      driveParser str `shouldBe` shown
    forM_
      -- differing precedence
      [ ("+, *", "1 + 2 * 3!", "(+ 1 (* 2 3))")
      , ("*, +", "1 * 2 + 3!", "(+ (* 1 2) 3)")
      , ("-, *", "1 - 2 * 3!", "(- 1 (* 2 3))")
      , ("*, -", "1 * 2 - 3!", "(- (* 1 2) 3)")
      , ("+, /", "1 + 2 / 3!", "(+ 1 (/ 2 3))")
      , ("/, +", "1 / 2 + 3!", "(+ (/ 1 2) 3)")
      , ("-, /", "1 - 2 / 3!", "(- 1 (/ 2 3))")
      , ("/, -", "1 / 2 - 3!", "(- (/ 1 2) 3)")
      , ("+, %", "1 + 2 % 3!", "(+ 1 (% 2 3))")
      , ("%, +", "1 % 2 + 3!", "(+ (% 1 2) 3)")
      , ("-, %", "1 - 2 % 3!", "(- 1 (% 2 3))")
      , ("%, -", "1 % 2 - 3!", "(- (% 1 2) 3)")
      ] $ \(name, str, shown) ->
      it ("should parse tokens of differing precedence: " ++ name) $
      driveParser str `shouldBe` shown
    forM_
      -- Parentheses
      [ ("with +", "1 + (2 + 3)!", "(+ 1 (+ 2 3))") -- otherwise would be an LL parse (+ (+ 1 2) 3)
      , ("with -", "1 - (2 + 3)!", "(- 1 (+ 2 3))")
      , ("with *", "1 * (2 + 3)!", "(* 1 (+ 2 3))")
      , ("with /", "1 / (2 + 3)!", "(/ 1 (+ 2 3))")
      , ("with %", "1 % (2 + 3)!", "(% 1 (+ 2 3))")
      , ("with ()", "1 + (2 + (3 + 4))!", "(+ 1 (+ 2 (+ 3 4)))")
      ] $ \(name, str, shown) ->
      it ("should parse parantheses with precendence: " ++ name) $
      driveParser str `shouldBe` shown
  describe "multiple expressions" $
    forM_
      [ ("on one line", "1! 2!", "1 2")
      , ("across multiple lines", "foo()! 1 + 2! 3!", "(foo ) (+ 1 2) 3")
      ] $ \(name, str, shown) ->
      it ("should parse multiple expressions: " ++ name) $
      driveParser str `shouldBe` shown
  describe "errors" $
    forM_
      [ ( "invalid expressions"
        , [ ("* 2!", "0:1")
          , ("5 + * 5!", "4:5")
          , ("+ * 5!", "2:3")
          , ("* (2 + 3)", "0:1")
          ]
        , "Error: expected an expression, got \"*\" at ")
      , ( "missing expression ending"
        , [ ("f()", "3:3")
          , ("name", "4:4")
          , ("4", "1:1")
          , ("4 + 5", "5:5")
          , ("(2 + 3)", "7:7")
          ]
        , "Error: expected an expression or delimiter, got \"<EOF>\" at ")
      , ( "invalid expression ending"
        , [ ("f(),", "3:4")
          , ("name,", "4:5")
          , ("4,", "1:2")
          , ("4 + 5,", "5:6")
          , ("(2 + 3),", "7:8")
          ]
        , "Error: expected \"!\", got \",\" at ")
      , ( "valid expression end but invalid parameter list delimiter or close"
        , [("f(1!", "3:4"), ("f(1, 2!", "6:7")]
        , "Error: expected \",\" or \")\", got \"!\" at ")
      , ( "invalid expression end in parameter list"
        , [("f(1]", "3:4"), ("f(1, 2]", "6:7")]
        , "Error: expected an expression or delimiter, got \"]\" at ")
      , ( "missing expression and invalid list close in parameter list"
        , [("f(]", "2:3")]
        , "Error: expected an expression, got \"]\" at ")
      , ( "invalid parantheses close"
        , [ ("(1!", "2:3")
          , ("(1 + 2!", "6:7")
          , ("(3 % 2!)", "6:7")
          , ("f((1!)", "4:5")
          , ("((1 + 2!)", "7:8")
          ]
        , "Error: expected \")\", got \"!\" at ")
      ] $ \(errorKind, cases, err) ->
      forM_ cases $ \(errCase, errSpan) ->
        it ("should reject " ++ errorKind ++ ": " ++ errCase) $
        driveParser errCase `shouldBe` err ++ errSpan
