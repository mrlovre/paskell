module Main where

import           Hash
import           System.Environment
import           System.IO

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering
    case args of
        [] -> runInteractive
        _ -> runScript $ head args

