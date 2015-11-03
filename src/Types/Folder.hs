{-# LANGUAGE OverloadedStrings #-}
module Types.Folder(
              FolderName,
              FileName,
              Folder(..),
              File(..),
              emptyFolder,
              modifyFolder,
              insertFileToFolder,
              createFolder,
              showFile,
              showFolder
              ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Aeson
import Data.Foldable (asum)

import Types.Hash

------------------------------- Datatypes -------------------------------------

type FolderName = Text
type FileName = Text

data Folder = Folder {
                folders :: Map Text Folder,
                files :: Map Text File
              }
data File = File Hash

------------------------ Folder manipulation ----------------------------------

emptyFolder :: Folder
emptyFolder = Folder M.empty M.empty

modifyFolder :: [FolderName] -> (Folder -> Folder) -> Folder -> Folder
modifyFolder [] f root                                  = f root
modifyFolder (folderName : fs) f (Folder folders files) =
      Folder (M.insert folderName updatedFolder folders) files
      where updatedFolder = modifyFolder fs f folder -- insert recursively as if that folder is the root.
            folder = M.findWithDefault emptyFolder folderName folders


-- insertFile: foo/bar/file.exe with hash. /file.exe
insertFileToFolder :: [FolderName] -> FileName -> File -> Folder -> Folder
insertFileToFolder pathToParent fileName (File hash) root = modifyFolder pathToParent addFile root
      where
        addFile (Folder folders files) = Folder folders (M.insert {-key=-}fileName {-value=-}(File hash) {-into:-}files)

createFolder :: [FolderName] -> Folder -> Folder
createFolder path root = modifyFolder path id root

--------------------------- Show Instances ------------------------------------

showFile :: Text -> File -> [Text]
showFile fileName (File fileHash) = [T.concat [fileName," = ", T.pack $ show fileHash]]

showFolder :: Text -> Folder -> [Text]
showFolder folderName (Folder folders files) = T.append folderName ":" : map (T.append "  ") (shownFiles ++ shownFolders)
      where
            shownFiles :: [Text]
            shownFiles = concat $ M.mapWithKey (showFile) files -- as if [Text]
            shownFolders :: [Text]
            shownFolders = concat $ M.mapWithKey (showFolder) folders -- as if [Text]


instance Show Folder where
  show folder = T.unpack $ T.intercalate "\n" (showFolder "root" folder)

instance Show File where
  show file = T.unpack $ T.intercalate "\n" (showFile "name?" file)

---------------------------- JSON Instances -----------------------------------

instance FromJSON File where
  parseJSON = withText "File" $ \fileHashS ->
                File <$> maybe (fail "Not a valid base 16 hash") return (readHash fileHashS)

instance FromJSON Folder where
  parseJSON = withObject "Folder" $ \o ->
      Folder <$> o .: "folders" <*> o .: "files"
--         Json -> Map Text Folder  Json -> Map Text File

instance ToJSON File where
  toJSON (File hash) = String hexaHash
    where hexaHash = T.pack (show hash)

instance ToJSON Folder where
  toJSON (Folder folders files) = object [
      "folders" .= folders,
      "files"   .= files -- (String, Map Text File -> Json)
    ]
