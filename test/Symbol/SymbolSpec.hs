module Symbol.SymbolSpec
  ( spec
  ) where

import Test.Hspec

import Symbol

import Control.Monad (forM_)

spec :: Spec
spec =
  describe "Symbol" $
  forM_
    [ (Symbol.IterUpTo, "IterUpTo")
    , (Symbol.NEq, "NEq")
    , (Symbol.GEq, "GEq")
    , (Symbol.LEq, "LEq")
    , (Symbol.Eq, "Eq")
    , (Symbol.Dot, "Dot")
    , (Symbol.LBracket, "LBracket")
    , (Symbol.RBracket, "RBracket")
    , (Symbol.LParen, "LParen")
    , (Symbol.RParen, "RParen")
    , (Symbol.LBrace, "LBrace")
    , (Symbol.RBrace, "RBrace")
    , (Symbol.TypeDelim, "TypeDelim")
    , (Symbol.ValueDelim, "ValueDelim")
    , (Symbol.Assign, "Assign")
    , (Symbol.GT, "GT")
    , (Symbol.LT, "LT")
    , (Symbol.Plus, "Plus")
    , (Symbol.Minus, "Minus")
    , (Symbol.Times, "Times")
    , (Symbol.Div, "Div")
    , (Symbol.Mod, "Mod")
    , (Symbol.ExprEnd, "ExprEnd")
    , (Symbol.In, "In")
    , (Symbol.And, "And")
    , (Symbol.Not, "Not")
    , (Symbol.Or, "Or")
    , (Symbol.For, "For")
    , (Symbol.While, "While")
    , (Symbol.If, "If")
    , (Symbol.Else, "Else")
    , (Symbol.ElseIf, "ElseIf")
    , (Symbol.Func, "Func")
    , (Symbol.Return, "Return")
    , (Symbol.Type, "Type")
    ] $ \(sym, str) -> it ("should show " ++ str) $ show sym `shouldBe` str
