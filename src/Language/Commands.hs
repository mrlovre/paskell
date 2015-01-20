module Language.Commands where

import           Control.Monad
import qualified Data.Map         as M
import           Language.Exec
import           System.Directory
import           System.Exit

{-|
    A map of (command name, command) pairs, used to abstract command
    execution and make adding new commands relatively easy
-}
commands :: CommandTable
commands = M.fromList [
    ("welcome", welcome),
    ("exit", exit),
    ("mv", move),
    ("cp", copy),
    ("cpdir", copyDir),
    ("rm", remove),
    ("rmdir", removeDir),
    ("create", create),
    ("mkdir", makeDir),
    ("pwd", pointWorkingDir),
    ("ls", list),
    ("cd", changeDir),
    ("cat", cat)
    ]

move, copy, remove, create, copyDir, removeDir, makeDir :: Command
move [] sstate = return sstate {output = "Missing source and destination."}
move [_] sstate = return sstate {output =  "Missing destination."}
move [src, target] sstate = do
    let src' = fixPath src sstate
        target' = fixPath target sstate
    renameFile src' target'
    putStrLn ""
    return sstate {output = "File successfully renamed."}
move plenty sstate = do
    createDirectoryIfMissing True dest
    let srcs' = map (`fixPath` sstate) srcs
    doWork srcs' where
    dest:srcs = reverse plenty
    doWork [] = return sstate {output = "Files successfully moved."}
    doWork (src:rest) = do
        dest' <- makeRelativeToCurrentDirectory dest
        let dest'' = fixPath dest' sstate
        renameFile src (dest'' ++ "/" ++ src)
        doWork rest

copy = undefined
remove = undefined
create [] sstate = return sstate {output = "Missing filename(s)."}
create files sstate = do
    let files' = map (`fixPath` sstate) files
    forM_ files' (`writeFile` "")
    return sstate {output = "File(s) successfully created."}

removeDir [] sstate = return sstate {output = "Missing directory name(s)."}
removeDir files sstate = do
    let files' = map (`fixPath` sstate) files
    forM_ files' removeDirectory
    return sstate {output = "Directory(es) successfully removed."}

copyDir = undefined

makeDir [] sstate = return sstate {output = "Missing directory name(s)."}
makeDir files sstate = do
    let files' = map (`fixPath` sstate) files
    forM_ files' $ createDirectoryIfMissing True
    return sstate {output = "Directory(es) successfully created."}

welcome, exit :: Command
welcome _ sstate = do
    putStrLn "Welcome to Interactive Hash!"
    return sstate

exit _ _ = do
    putStrLn "Bye! Haskell loves you!"
    exitSuccess

pointWorkingDir, list, changeDir :: Command
pointWorkingDir _ sstate = do
    let workingDir = wd sstate
    return sstate {output = workingDir}

list dir sstate = do
    let dir' = case dir of
            [] -> "."
            _  -> head dir
    dirList <- getDirectoryContents $ fixPath dir' sstate
    return sstate {output = unlines dirList}

changeDir dir sstate = do
    let dir' = case dir of
            [] -> "~"
            _ -> fixPath (head dir) sstate
    when (dir' == "~") $ do
        dir2 <- getHomeDirectory
        setCurrentDirectory dir2
    dir'' <- getCurrentDirectory
    return sstate {wd = dir''}

cat :: Command
cat [] sstate = return sstate {output = "Missing file name(s)."}
cat files sstate = do
    let files' = map (`fixPath` sstate) files
    forM_ files' ( \f -> do
        cont <- readFile f
        putStrLn cont)
    return sstate {output = ""}

fixPath :: FilePath -> ScriptState -> FilePath
fixPath path ss =
    let workingDir = wd ss in
    case path of
        ('/':_) -> path
        _ -> workingDir ++ "/" ++ path
