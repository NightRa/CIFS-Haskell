module Demos.Demo3 (indexFolder) where

import Control.Monad
import System.Directory
import System.Directory.Extras
import System.FilePath
import qualified Data.Text as T

import Types.Index
import Types.Folder

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
