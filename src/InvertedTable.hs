{-# LANGUAGE OverloadedStrings #-}

module InvertedTable(
                    InvertedTable(..),
                    insertFileToInvertedTable,
                    emptyInvertedTable
                    ) where

import Data.Trie
import Folder
import Util.Show
import Data.List (sortOn)
--  Map: bytestring -> filepath
data InvertedTable = InvertedTable (Trie FilePath)

insertFileToInvertedTable :: Hash -> FilePath -> InvertedTable -> InvertedTable
insertFileToInvertedTable (Hash hash) filePath (InvertedTable table) = InvertedTable $ insert hash filePath table

emptyInvertedTable :: InvertedTable
emptyInvertedTable = InvertedTable empty

instance Show InvertedTable where
  show (InvertedTable trie) =
    mkString' "\n" (\(hash, path) -> show (Hash hash) ++ " -> " ++ path) (sortOn snd (toList trie))
