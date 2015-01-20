module Main where

import           Hash
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runInteractive
