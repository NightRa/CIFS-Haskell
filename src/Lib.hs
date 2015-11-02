{-# LANGUAGE OverloadedStrings #-}
module Lib(
          hashFile,
          readFolderStructure,
          getFiles,
          getFolders,
          indexFolder
          ) where

import Control.Monad
import System.Directory
import System.FilePath
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Folder
import Index
import IO.Hashing

-- Note: Lazy IO gives us an open handle leakage. Need to fix.

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

indexFolder :: FilePath -> IO Index
indexFolder path = do
    absFiles <- getAbsoluteFilesRecursively path
    foldM insertToIndex emptyIndex absFiles
  where insertToIndexId :: FilePath -> [FolderName] -> FileName -> Index -> IO Index
        insertToIndexId = insertFile (\filePath _ -> return filePath) -- Notice: prototype 3 behavior!
        insertToIndex :: Index -> FilePath -> IO Index
        insertToIndex index file = insertToIndexId file (T.pack <$> relFolderPath file) (T.pack $ fileName file) index
        relFolderPath :: FilePath -> [FilePath]
        relFolderPath file = splitDirectories $ makeRelative path file
        fileName :: FilePath -> FilePath
        fileName file = takeFileName file


readFolderStructure :: FilePath -> IO Folder
readFolderStructure path = do
    fileNames <- getFiles path
    folderNames <- getFolders path
    fileHashes <- traverse (\fileName -> do
                              hash <- hashFile fileName
                              return (T.pack (takeFileName fileName), File hash)) fileNames
    indexedFolders <- traverse (\folderName ->
                                  do
                                    folderIndex <- readFolderStructure folderName
                                    return (T.pack (last . splitDirectories $ takeDirectory folderName), folderIndex)) folderNames
    return $ Folder (M.fromList indexedFolders) (M.fromList fileHashes)
