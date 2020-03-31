module Main where

import qualified Lib as LibHgb
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    input <- getContents
    let lexed = LibHgb.lex input
    if args == ["--lex"]
        then print lexed
        else print $ LibHgb.parse lexed

