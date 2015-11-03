{-# LANGUAGE OverloadedStrings #-}
module Types.Index(
            Index(..),
            insertFile,
            emptyIndex
            ) where

import Types.InvertedTable
import Types.Folder
import Types.Hash
import IO.Hashing

------------------------------- Datatypes -------------------------------------

data Index = Index {
               rootFolder :: Folder,
               invertedTable :: InvertedTable
             }

-------------------------- Index manipulation ---------------------------------

emptyIndex :: Index
emptyIndex = Index emptyFolder emptyInvertedTable

-- save :: FilePath -> Hash -> IO FilePath
insertFile :: (FilePath -> Hash -> IO FilePath) -> FilePath -> [FolderName] -> FileName -> Index -> IO Index
insertFile save filePathInDisk parentPath fileName (Index root invertedTable) = do
  hash <- hashFile filePathInDisk
  newFilePathInDisk <- save filePathInDisk hash
  let newRootFolder = insertFileToFolder parentPath fileName (File hash) root
  let newInvertedTable = insertFileToInvertedTable hash newFilePathInDisk invertedTable
  return $ Index newRootFolder newInvertedTable
