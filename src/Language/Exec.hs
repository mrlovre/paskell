module Language.Exec where

import           Control.Monad
import           Data.List
import           Data.Map             as M
import           Language.Expressions
import           System.Directory

{-|
    A model of a command which is waiting for arguments and a state to run.
-}
type Command = [String] -> ScriptState -> IO ScriptState

{-|
    A table of variables, in fact a map of (Name, Value) pairs.
-}
type VarTable = M.Map String String

{-|
    A command table - abstracted command execution, contains (command name,
    command) pairs. Simplest, yet the best way to implement this =P.
-}
type CommandTable = M.Map String Command

{-|
    A script state containing the last output, current working directory and
    the current table of variables.
-}
data ScriptState = ScriptState {
    output   :: String,
    wd       :: FilePath,
    vartable :: VarTable
} deriving Show

createInitialScriptState :: IO ScriptState
createInitialScriptState = do
    workingDir <- getCurrentDirectory
    return ScriptState {output = "", wd = workingDir, vartable = M.empty}


{-|
    Runs a set of commands for a given command table. If this is the first
    command in the chain, it is given a FilePath and constructs a new, initially
    blank, ScriptState. Otherwise, it is given the state as left by the previous
    command's execution.
-}
runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState
runHashProgram = undefined

{-|
    Calculates the result of a top-level command execution
-}
runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState
runTopLevel ct ss (TLCmd cmdData) = do
    nss <- executeCmd cmdData ct ss
    putStrLn $ output nss
    return nss

runTopLevel ct ss (TLCnd tlcnd) = do
    case tlcnd of
        If cnd cthn ->
            if evaluate cnd then do
                results <- forM cthn ( \x -> executeCmd x ct ss)
                return $ last results
            else
                return ss
        IfElse cnd cthn cels ->
            if evaluate cnd then do
                results <- forM cthn ( \x -> executeCmd x ct ss)
                return $ last results
            else do
                results <- forM cels ( \x -> executeCmd x ct ss)
                return $ last results

executeCmd :: Cmd -> CommandTable -> ScriptState -> IO ScriptState
executeCmd cmdData cmdTable ss = do
    let cmdName = name cmdData
        cmd = M.lookup cmdName cmdTable
    case cmd of
        Just cmd' -> do
            let arguments = args cmdData
            cmd' arguments ss
        Nothing ->
            return ss {output = "Command " ++ cmdName ++ " not defined."}

{-
    The rest of the module should consist of similar functions, calling each
    other so that each expression is parsed by a lower-level function and the
    result can be used in a higher-level function. The Command table and state
    are passed around as necessary to evaluate commands, assignments and
    variable substitution. A better way to pass around variables would be to
    use the State monad or even the StateT monad transformer to wrap IO into it.
-}
