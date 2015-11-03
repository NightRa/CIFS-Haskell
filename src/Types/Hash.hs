{-# LANGUAGE OverloadedStrings #-}
module Types.Hash(
             Hash(..),
             hashSize,
             readHash
             ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Ch8
import qualified Data.ByteString.Base16 as BS16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

------------------------------- Datatypes -------------------------------------

-- | A Hash of some data, currently is SHA3-512
newtype Hash = Hash ByteString

-- | This is a constant for the time being, will be generalized at some point.
hashSize :: Int
hashSize = 512

-------------------------- Show/Read Instances --------------------------------

instance Show Hash where
  show (Hash hash) = Ch8.unpack $ BS16.encode hash

readHash :: Text -> Maybe Hash
readHash fileHashS = assertCorrect base16O
  where base16O = BS16.decode $ T.encodeUtf8 fileHashS
        assertCorrect (base16, "") | BS.length base16 == hashSize = Just $ Hash base16
        assertCorrect _ = Nothing
