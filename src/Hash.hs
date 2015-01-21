module Hash where

import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Language.Commands
import           Language.Exec
import           Language.Expressions
import           System.Environment
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
runScript fp = do
    cont <- readFile fp
    initss <- createInitialScriptState
    let exprs = map ( `parse` initss) (lines cont ++ ["exit"])
    foldM_ (runTopLevel commands) initss exprs

{-|
    Communicates with the user and performs hash commands line by line
-}
runInteractive :: IO ()
runInteractive = do
    initialSS <- createInitialScriptState
    initss <- runTopLevel commands initialSS (TLCmd defaultCommand)
    doWork initss where
    doWork ss = (do
        let workDir = wd ss
        putStr $ workDir ++ "> "
        hFlush stdout
        line <- getLine
        let expr = parse line ss
        nss <- runTopLevel commands ss expr
        doWork nss) `catch` ( \(SomeException e) ->
            (unless (show e == "ExitSuccess") $ do
                putStrLn $ "An error occurred: " ++ show e ++ "."
                doWork ss))

parse :: String -> ScriptState -> TLExpr
parse line sstate =
    let line' = removeComments line
        chunks = words line' in
    case chunks of
        "if":_ -> TLCnd (parseCnd chunks sstate)
        _ -> TLCmd (parseCmd chunks)

parseCnd :: [String] -> ScriptState -> Conditional
parseCnd ss sstate =
    let ss' = ifThenElse ss sstate in
    case ss' of
        (cond', cthen', []) -> If cond' cthen'
        (cond', cthen', celse') -> IfElse cond' cthen' celse'

ifThenElse :: [String] -> ScriptState -> (Pred, [Cmd], [Cmd])
ifThenElse ss sstate = (cond', cthen', celse') where
    (c, ss') = span (/= "then") (drop 1 ss)
    (t, _:e) = span (/= "else") (drop 1 ss' ++ [" "])
    cond' = parsePred c sstate
    cthen' = map (parseCmd . words) $ (wordsWhen (==';') . unwords) t
    celse' = map (parseCmd . words) $ (wordsWhen (==';') . unwords) e

parsePred :: [String] -> ScriptState -> Pred
parsePred ss sstate = Pred (parseComp ss sstate)


parseCmd :: [String] -> Cmd
parseCmd chunks =
    case chunks of
        [] -> defaultCommand
        (n:a) -> Cmd {name=n, args=a, inDir=Nothing, outDir=Nothing, append=False}

parseComp :: [String] -> ScriptState -> Comp
parseComp ss sstate
    | "==" `elem` ss = let (s1, s2) = parseVars "==" ss sstate in CEQ s1 s2
    | "/=" `elem` ss = let (s1, s2) = parseVars "/=" ss sstate in CNE s1 s2
    | ">=" `elem` ss = let (s1, s2) = parseVars ">=" ss sstate in CGE s1 s2
    | ">" `elem` ss = let (s1, s2) = parseVars ">" ss sstate in CGT s1 s2
    | "<=" `elem` ss = let (s1, s2) = parseVars "/=" ss sstate in CLE s1 s2
    | "<" `elem` ss = let (s1, s2) = parseVars "/=" ss sstate in CLT s1 s2

parseVars :: String -> [String] -> ScriptState -> (String, String)
parseVars delim ss sstate =
    let (s1, s') = span (/= delim) ss
        s2 = drop 1 s'
        s1' = Hash.escapeVariable (concat s1) sstate
        s2' = Hash.escapeVariable (concat s2) sstate in
    (s1', s2')

escapeVariable :: String -> ScriptState -> String
escapeVariable sv sstate =
    case sv of
        '$':vname ->
            let vt = vartable sstate in
            fromMaybe "" (M.lookup vname vt)
        _ -> sv


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
