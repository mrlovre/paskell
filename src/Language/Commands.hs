module Language.Commands where

import           Control.Monad
import qualified Data.Map           as M
import           Data.Maybe
import           Language.Exec
import           System.Cmd
import           System.Directory
import           System.Exit
import           System.Posix.Files

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
    ("cat", cat),
    ("echo", echo),
    ("assign", assign),
    ("ping", ping),
    ("chmod", chmod)x
    ]

move, copy, remove, create, copyDir, removeDir, makeDir :: Command
move [] sstate = return sstate {output = "Missing source and destination."}
move [_] sstate = return sstate {output =  "Missing destination."}
move [src, target] sstate = do
    let src' = fixPath src sstate
        target' = fixPath target sstate
    renameFile src' target'
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

copy [] sstate = return sstate {output = "Missing source and destination."}
copy [_] sstate = return sstate {output = "Missing destination."}
copy [src, target] sstate = do
    let src' = fixPath src sstate
        target' = fixPath target sstate
    copyFile src' target'
    return sstate {output = "File successfully copied."}
copy plenty sstate = do
    createDirectoryIfMissing True dest
    let srcs' = map (`fixPath` sstate) srcs
    doWork srcs' where
        dest:srcs = reverse plenty
        doWork [] = return sstate {output = "Files successfully copied."}
        doWork (src:rest) = do
            dest' <- makeRelativeToCurrentDirectory dest
            let dest'' = fixPath dest' sstate
            copyFile src (dest'' ++ "/" ++ src)
            doWork rest

remove [] sstate = return sstate {output = "Missing filename(s)."}
remove files sstate = do
    let files' = map (`fixPath` sstate) files
    forM_ files' removeFile
    return sstate {output = "File(s) successfully deleted."}

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

copyDir [] sstate = return sstate {output = "Missing source name."}
copyDir [_] sstate = return sstate {output = "Missing destination name."}
copyDir [src, dest] sstate = do
    let src' = fixPath src sstate
        dest' = fixPath dest sstate
    cpdir src' dest'
    return sstate {output = "Directory succesfully copied."}

copyDir plenty sstate = do
    createDirectoryIfMissing True dest
    let srcs' = map (`fixPath` sstate) srcs
    doWork srcs' where
        dest:srcs = reverse plenty
        doWork [] = return sstate {output = "Files successfully copied."}
        doWork (src:rest) = do
            cpdir src dest
            doWork rest

cpdir :: FilePath -> FilePath -> IO ()
cpdir src dest = do
    createDirectoryIfMissing True dest
    dest' <- makeRelativeToCurrentDirectory dest
    names <- getDirectoryContents src
    forM_ names ( \f -> do
        copyFile (src ++ "/" ++ f) (dest' ++ "/" ++ f))

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
    if (dir' == "~")
        then do
            dir2 <- getHomeDirectory
            setCurrentDirectory dir2
        else do
            setCurrentDirectory dir'
    dir'' <- getCurrentDirectory
    return sstate {wd = dir''}

assign :: Command
assign [varname, value] sstate = do
    let vt = vartable sstate
    return sstate {vartable = M.insertWith const varname value vt}

assign _ sstate = return sstate {output = "Missing arguments."}

echo :: Command
echo args sstate = do
    putStrLn $ unwords $ map (`escapeVariable` sstate) args
    return $ sstate{output = ""}

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


escapeVariable :: String -> ScriptState -> String
escapeVariable sv sstate =
    case sv of
        '$':vname ->
            let vt = vartable sstate in
            fromMaybe "" (M.lookup vname vt)
        _ -> sv

ping, chmod, hexdump :: Command

ping [] sstate = return sstate {output="Missing arguments"}
ping args sstate = do
    system ("ping " ++ head args)
    return sstate

chmod [file, perms] sstate = do
    let mode =
            case perms of
                "+x" -> ownerExecuteMode `unionFileModes` groupExecuteMode `unionFileModes` otherExecuteMode
                "+w" -> ownerWriteMode `unionFileModes` groupWriteMode `unionFileModes` otherWriteMode
                "+r" -> ownerReadMode `unionFileModes` groupReadMode `unionFileModes` otherReadMode
    setFileMode file mode
    return sstate{output="Mode changed."}



chmod _ sstate = return sstate {output="Missing arguments"}

hexdump = undefined
