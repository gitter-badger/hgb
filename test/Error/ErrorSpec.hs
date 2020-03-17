module Error.ErrorSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Symbol (Symbol(..))
import Test.Hspec

import Error (Error(..), ErrorType(..))

spec :: Spec
spec = do
  describe "ErrorType" $
    describe "ExpectedSymbol" $
    it "should acess fileds correctly" $ do
      let es = ExpectedSymbol {expected = [Symbol.Plus], actual = Symbol.Minus}
      (show (expected es), show (actual es)) `shouldBe` ("[Plus]", "Minus")
  describe "Error" $
    forM_
      [ ( "expected symbol"
        , ExpectedSymbol {expected = [Symbol.Plus], actual = Symbol.Minus}
        , "Error: expected Plus; got Minus")
      ] $ \(name, err, str) ->
      it ("should show errors: " ++ name) $ show (Error err) `shouldBe` str
