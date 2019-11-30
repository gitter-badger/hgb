module Main where

import qualified Lib as LibHgb

main :: IO ()
main = do
    print (LibHgb.lex "=/=")
