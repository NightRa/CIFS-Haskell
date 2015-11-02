{-# LANGUAGE OverloadedStrings #-}
module Index(
            Index(..),
            insertFile,
            emptyIndex
            ) where

import InvertedTable
import Folder
import IO.Hashing

data Index = Index {
               rootFolder :: Folder,
               invertedTable :: InvertedTable
             }

-- save :: FilePath -> Hash -> IO FilePath
insertFile :: (FilePath -> Hash -> IO FilePath) -> FilePath -> [FolderName] -> FileName -> Index -> IO Index
insertFile save filePathInDisk parentPath fileName (Index root invertedTable) = do
  hash <- hashFile filePathInDisk
  newFilePathInDisk <- save filePathInDisk hash
  let newRootFolder = insertFileToFolder parentPath fileName (File hash) root
  let newInvertedTable = insertFileToInvertedTable hash newFilePathInDisk invertedTable
  return $ Index newRootFolder newInvertedTable

emptyIndex :: Index
emptyIndex = Index emptyFolder emptyInvertedTable
