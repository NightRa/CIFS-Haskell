{-# LANGUAGE OverloadedStrings #-}
module Lib
    (hashFile
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Crypto.Hash.SHA3

hashFile :: FilePath -> IO ByteString
hashFile file = do
  fileContents <- LBS.readFile file
  return $ hashlazy 128 fileContents
