module Hash where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Language.Commands
import           Language.Exec
import           Language.Expressions
import           System.IO
import           Text.Regex

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
    initss <- runTopLevel commands initialSS (TLCmd defaultCommand)
    doWork initss where
    doWork :: ScriptState -> IO ()
    doWork ss = (do
        let workDir = wd ss
        putStr $ workDir ++ "> "
        hFlush stdout
        line <- getLine
        let expr = parse line
        nss <- runTopLevel commands ss expr
        doWork nss) `catch` ( \(SomeException e) ->
            (unless (show e == "ExitSuccess") $ do
                putStrLn $ "An error occurred: " ++ show e ++ "."
                doWork ss))

parse :: String -> TLExpr
parse line =
    let line' = removeComments line
        chunks = words line' in
    case chunks of
        "if":_ -> TLCnd (parseCnd chunks)
        _ -> TLCmd (parseCmd chunks)

parseCnd :: [String] -> Conditional
parseCnd ss =
    let ss' = ifThenElse ss in
    case ss' of
        (cond', cthen', []) -> If cond' cthen'
        (cond', cthen', celse') -> IfElse cond' cthen' celse'

ifThenElse :: [String] -> (Pred, [Cmd], [Cmd])
ifThenElse ss = (cond', cthen', celse') where
    (c, ss') = span (/= "then") (drop 1 ss)
    (t, _:e) = span (/= "else") (drop 1 ss')
    cond' = parsePred c
    cthen' = map (parseCmd . words) $ (wordsWhen (==';') . unwords) t
    celse' = map (parseCmd . words) $ (wordsWhen (==';') . unwords) e

parsePred :: [String] -> Pred
parsePred ss
    | "==" `elem` ss = Pred (CEQ s1 s2) where (s1, s2) = span (/= "==") ss


parseCmd :: [String] -> Cmd
parseCmd chunks =
    case chunks of
        [] -> defaultCommand
        (n:a) -> Cmd {name=n, args=a, inDir=Nothing, outDir=Nothing, append=False}

parseComp :: [String] -> Comp
parseComp ss =
    

defaultCommand :: Cmd
defaultCommand = Cmd {name="welcome", args=[], inDir=Nothing, outDir=Nothing, append=False}

removeComments :: String -> String
removeComments = takeWhile (/= '#')

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s'' where
            (w, s'') = break p s'
