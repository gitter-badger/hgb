module Token.TokenSpec
  ( spec
  ) where

import Test.Hspec

import Symbol (Symbol(..))
import Token (Token(..))
import Utils (Span(..))

spec :: Spec
spec = do
  it "should show tokens correctly" $
    show (Token Symbol.String "str" (Span 0 3)) `shouldBe`
    "Token {symbol = String, content = \"str\", sourceSpan = 0:3}"
  it "should access token data fields correctly" $ do
    let t = Token Symbol.Name "zifah" (Span 5 10)
    (symbol t, content t, sourceSpan t) `shouldBe`
      (Symbol.Name, "zifah", Span 5 10)
