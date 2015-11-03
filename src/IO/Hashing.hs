module IO.Hashing (hashFile) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Crypto.Hash.SHA3

import Types.Folder
import Types.Hash

hashFile :: FilePath -> IO Hash
hashFile file = do
  fileContents <- LBS.readFile file
  return $ Hash $ hashlazy 128 fileContents
