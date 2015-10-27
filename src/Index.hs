{-# LANGUAGE OverloadedStrings #-}
module Index(
             Index(..)
            ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as BS16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Foldable (asum)

data Index = Folder {
              folderName :: Text,
              folderEntries :: [Index]
             } |
             File {
               fileName :: Text,
               fileHash :: ByteString
             }

showIndex :: Index -> [Text]
showIndex (Folder folderName folderEntries) = T.append folderName ":" : map (T.cons '\t') (concat $ showIndex <$> folderEntries)
showIndex (File fileName fileHash) = [T.concat [fileName," = ", T.decodeUtf8 $ BS16.encode fileHash]]

instance Show Index where
  show index = T.unpack $ T.intercalate "\n" (showIndex index)

instance FromJSON Index where
  parseJSON = withObject "Index: Folder or File" $ \o -> asum [
      Folder <$> o .: "folderName" <*> o .: "folderEntries",
      File <$> o .: "fileName" <*> do
                                    fileHashS <- o .: "fileHash"
                                    let base16O = BS16.decode $ T.encodeUtf8 fileHashS
                                    assertCorrect base16O
    ]
    where assertCorrect (base16, "") | BS.length base16 == 512 = return base16
          assertCorrect _ = fail "Not base 16 at length 512"

instance ToJSON Index where
  toJSON (Folder folderName folderEntries) = object [
      "folderName" .= folderName,
      "folderEntries" .= folderEntries
    ]
  toJSON (File fileName fileHash) = object [
      "fileName" .= fileName,
      "fileHash" .= T.decodeUtf8 (BS16.encode fileHash)
    ]
