{-# LANGUAGE OverloadedStrings #-}
module Index(
            FolderName,
            FileName,
            Hash,
            Folder(..),
            File(..),
            emptyFolder,
            modifyFolder,
            insertFile,
            createFolder,
            showFile,
            showFolder
            ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as BS16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Aeson
import Data.Foldable (asum)

type FolderName = Text
type FileName = Text
type Hash = ByteString

hashSize :: Int
hashSize = 512

data Folder = Folder {
                folders :: Map Text Folder,
                files :: Map Text File
              }
data File = File Hash

emptyFolder :: Folder
emptyFolder = Folder M.empty M.empty

modifyFolder :: [FolderName] -> (Folder -> Folder) -> Folder -> Folder
modifyFolder [] f root                                  = f root
modifyFolder (folderName : fs) f (Folder folders files) =
      Folder (M.insert folderName updatedFolder folders) files
      where updatedFolder = modifyFolder fs f folder -- insert recursively as if that folder is the root.
            folder = M.findWithDefault emptyFolder folderName folders


-- insertFile: foo/bar/file.exe with hash. /file.exe
insertFile :: [FolderName] -> FileName -> File -> Folder -> Folder
insertFile pathToParent fileName (File hash) root = modifyFolder pathToParent addFile root
      where addFile (Folder folders files) = Folder folders (M.insert {-key=-}fileName {-value=-}(File hash) {-into:-}files)

createFolder :: [FolderName] -> Folder -> Folder
createFolder path root = modifyFolder path id root

showFile :: Text -> File -> [Text]
showFile fileName (File fileHash) = [T.concat [fileName," = ", T.decodeUtf8 $ BS16.encode fileHash]]

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

instance FromJSON File where
  parseJSON = withText "File" $ \fileHashS ->
                File <$>  let base16O = BS16.decode $ T.encodeUtf8 fileHashS in
                          assertCorrect base16O
              where assertCorrect (base16, "") | BS.length base16 == hashSize = return base16
                    assertCorrect _ = fail $ "Not base 16 at length " ++ show hashSize

instance FromJSON Folder where
  parseJSON = withObject "Folder" $ \o ->
      Folder <$> o .: "folders" <*> o .: "files"
--         Json -> Map Text Folder  Json -> Map Text File

instance ToJSON File where
  toJSON (File hash) = String hexaHash
    where hexaHash = T.decodeUtf8 (BS16.encode hash)

instance ToJSON Folder where
  toJSON (Folder folders files) = object [
      "folders" .= folders,
      "files"   .= files -- (String, Map Text File -> Json)
    ]
