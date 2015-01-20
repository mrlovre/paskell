module Hash where

import           Language.Commands
import           Language.Exec
import           Language.Expressions
import           System.IO
import           System.Path.Glob

{-|
    The top-level module. Connects parsing to execution and adds interaction
    with the user / reading from file.
-}

{-|
    Runs a .hash script
-}
runScript :: FilePath -> IO ()
runScript = undefined

{-|
    Communicates with the user and performs hash commands line by line
-}
runInteractive :: IO ()
runInteractive = do
    initialSS <- createInitialScriptState
    initss <- runTopLevel commands initialSS (TLCmd Cmd {name = "welcome", args = []})
    doWork initss where
    doWork ss = do
        let workDir = wd ss
        putStr $ workDir ++ "> "
        hFlush stdout
        line <- getLine
        let (n:a) = words line
            expr = TLCmd Cmd {name = n, args = a}
        nss <- runTopLevel commands ss expr
        doWork nss
