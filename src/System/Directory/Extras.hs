module System.Directory.Extras(
                              getFiles,
                              getFolders,
                              getAbsoluteFilesRecursively
                              ) where

import Control.Monad
import System.Directory
import System.FilePath

-- Note: TODO: Lazy IO gives us an open handle leakage. Need to fix.

getFiles :: FilePath -> IO [FilePath]
getFiles path = do
  contents' <- getDirectoryContents path
  let contents = filter (\path -> path /= "." && path /= "..") contents'
      contentPaths = (path </>) <$> contents
  filterM doesFileExist contentPaths

getFolders :: FilePath -> IO [FilePath]
getFolders path = do
  contents' <- getDirectoryContents path
  let contents = filter (\path -> path /= "." && path /= "..") contents'
      contentPaths = (\name -> addTrailingPathSeparator (path </> name)) <$> contents
  filterM doesDirectoryExist contentPaths

{-
x.exe
foo/
  bar/a.exe
  b.exe
=>
x.exe
foo/bar/a.exe
foo/b.exe
-}
getAbsoluteFilesRecursively :: FilePath -> IO [FilePath]
getAbsoluteFilesRecursively path =
  do
    rootFiles <- getFiles path
    folderNames <- getFolders path
    -- [a] -> (a -> IO [b]) -> IO [b]
    -- traverse -> IO [[b]] -> map (join) -> IO [b]
    recFiles <- join <$> traverse (getAbsoluteFilesRecursively) folderNames
    -- IO [a] -> (a -> IO [b]) -> IO [b]
    -- Can use a monad transformer on IO [a]!
    -- f a = IO [a] -- Has a monad transormer
    return $ rootFiles ++ recFiles
