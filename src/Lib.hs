{-# LANGUAGE OverloadedStrings #-}
module Lib(
          hashFile,
          indexFolder,
          getFiles,
          getFolders
          ) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Crypto.Hash.SHA3
import System.Directory
import System.FilePath
import Index
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- Note: Lazy IO gives us an open handle leakage. Need to fix.

hashFile :: FilePath -> IO Hash
hashFile file = do
  fileContents <- LBS.readFile file
  return $ hashlazy 128 fileContents

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


indexFolder :: FilePath -> IO Folder
indexFolder path = do
    fileNames <- getFiles path
    folderNames <- getFolders path
    fileHashes <- traverse (\fileName -> do
                              hash <- hashFile fileName
                              return (T.pack (takeFileName fileName), File hash)) fileNames
    indexedFolders <- traverse (\folderName ->
                                  do
                                    folderIndex <- indexFolder folderName
                                    return (T.pack (last . splitDirectories $ takeDirectory folderName), folderIndex)) folderNames
    return $ Folder (M.fromList indexedFolders) (M.fromList fileHashes)
