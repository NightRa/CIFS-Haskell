{-# LANGUAGE OverloadedStrings #-}
module Lib
    (hashFile
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Crypto.Hash.SHA3
import Index (Hash)

hashFile :: FilePath -> IO Hash
hashFile file = do
  fileContents <- LBS.readFile file
  return $ hashlazy 128 fileContents
