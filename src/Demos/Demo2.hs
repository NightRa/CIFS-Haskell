module Demos.Demo2 (readFolderStructure) where

import System.Directory.Extras
import IO.Hashing
import qualified Data.Text as T
import System.FilePath
import Types.Folder
import qualified Data.Map.Strict as M

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
