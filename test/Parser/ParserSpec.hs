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
  describe "function definitions" $ do
    it "should parse function definitions with parameters" $
      driveParser "fun PRINT(a : int) : void { hey! 1 + 1!}" `shouldBe`
      "(fun (PRINT ((declare ($a : int))) : void) ($hey (+ 1 1)))"
    it "should parse function definitions with no parameters" $
      driveParser "fun FOO() : void {a : int!}" `shouldBe`
      "(fun (FOO () : void) ((declare ($a : int))))"
  describe "variable declarations" $
    forM_
      [ ("with no initializer", "a:int!", "(declare ($a : int))")
      , ( "with an initializer"
        , "foo:uint64 = 10!"
        , "(declare ($foo : uint64) 10)")
      , ( "with an expression initializer"
        , "foo:uint64 = 10 + 20 * 30!"
        , "(declare ($foo : uint64) (+ 10 (* 20 30)))")
      , ("with arbitrary whitespace", "a  : \t void!", "(declare ($a : void))")
      ] $ \(name, input, output) ->
      it ("should parse variable declations " ++ name) $
      driveParser input `shouldBe` output
  describe "variable assignments" $
    forM_
      [ ("to another variable", "a = b!", "(= $a $b)")
      , ("to an expression", "a = 1 + 1!", "(= $a (+ 1 1))")
      , ("to a function call", "a = foo(a)!", "(= $a (foo $a))")
      ] $ \(name, input, output) ->
      it ("should parse variable assignments " ++ name) $
      driveParser input `shouldBe` output
  describe "return statements" $
    it "should parse return statments" $
    driveParser "return 1 + 1!" `shouldBe` "(return (+ 1 1))"
  describe "calls" $ do
    forM_
      [ ("without arguments", "foo()!", "(foo )")
      , ("with one argument", "foo(1)!", "(foo 1)")
      , ( "with multiple arguments"
        , "foo(1, data, \"str\")!"
        , "(foo 1 $data \"str\")")
      , ("with multiple arguments", "foo(f())!", "(foo (f ))")
      , ("with expression parameters", "foo(1 + 2 * 3)!", "(foo (+ 1 (* 2 3)))")
      ] $ \(name, str, shown) ->
      it ("should parse calls: " ++ name) $ driveParser str `shouldBe` shown
    describe "prefix sign calls" $ do
      it "should parse positive calls" $ driveParser "+2!" `shouldBe` "(+ 2)"
      it "should parse negative calls" $ driveParser "-2!" `shouldBe` "(- 2)"
      it "should parse chained infix calls" $
        driveParser "-+-2!" `shouldBe` "(- (+ (- 2)))"
      it "should parse prefix calls with variables" $ driveParser "+name!" `shouldBe` "(+ $name)"
      it "should parse prefix calls with declarations" $ driveParser "+a : int = 1!" `shouldBe` "(+ (declare ($a : int) 1))"
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
        , "Error: expected an operator or \"!\", got \"<EOF>\" at ")
      , ( "invalid expression ending"
        , [ ("f(),", "3:4")
          , ("name,", "4:5")
          , ("4,", "1:2")
          , ("4 + 5,", "5:6")
          , ("(2 + 3),", "7:8")
          ]
        , "Error: expected an operator or \"!\", got \",\" at ")
      , ( "valid expression end but invalid parameter list delimiter or close"
        , [("f(1!", "3:4"), ("f(1, 2!", "6:7")]
        , "Error: expected an operator, \",\", or \")\", got \"!\" at ")
      , ( "invalid expression end in parameter list"
        , [("f(1]", "3:4"), ("f(1, 2]", "6:7")]
        , "Error: expected an operator, \",\", or \")\", got \"]\" at ")
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
        , "Error: expected an operator or \")\", got \"!\" at ")
      , ( "invalid assignments"
        , [("1 + 1 = a!", "6:7")]
          -- TODO: A better message may say that an expression cannot be on the LHS of an assignment.
        , "Error: expected an operator or \"!\", got \"=\" at ")
      , ( "declaration with invalid initializations"
        , [("a : int +!", "8:9")]
        , "Error: expected an assignment or \"!\", got \"+\" at ")
      , ( "declarations with no type name"
        , [("a : !", "4:5")]
        , "Error: expected \"<Type>\", got \"!\" at ")
      , ( "declarations with invalid type names"
        , [("a : 1!", "4:5")]
        , "Error: expected \"<Type>\", got \"1\" at ")
      , ( "functions declarations with no expressions"
        , [("fun foo () : void {}", "19:20")]
        , "Error: expected an expression, got \"}\" at ")
      , ( "functions declarations with invalid names"
        , [("fun 1 () : int {return 1!}", "4:5")]
        , "Error: expected a name, got \"1\" at ")
      , ( "functions declarations with no return type"
        , [("fun foo () {return 1!}", "11:12")]
        , "Error: expected \":\", got \"{\" at ")
      , ( "functions declarations with no return type name"
        , [("fun foo () : {return 1!}", "13:14")]
        , "Error: expected \"<Type>\", got \"{\" at ")
      , ( "functions declarations with invalid return type names"
        , [("fun foo () : {return 1!}", "13:14")]
        , "Error: expected \"<Type>\", got \"{\" at ")
      ] $ \(errorKind, cases, err) ->
      forM_ cases $ \(errCase, errSpan) ->
        it ("should reject " ++ errorKind ++ ": " ++ errCase) $
        driveParser errCase `shouldBe` err ++ errSpan
