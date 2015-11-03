{-# LANGUAGE OverloadedStrings #-}

module Types.InvertedTable(
                    InvertedTable(..),
                    insertFileToInvertedTable,
                    emptyInvertedTable
                    ) where

import Data.Trie
import Data.List (sortOn)
import Util.Show

import Types.Folder
import Types.Hash

------------------------------- Datatypes -------------------------------------

--  Map: bytestring -> filepath
data InvertedTable = InvertedTable (Trie FilePath)

------------------------ Inverted Table manipulation --------------------------

emptyInvertedTable :: InvertedTable
emptyInvertedTable = InvertedTable empty

insertFileToInvertedTable :: Hash -> FilePath -> InvertedTable -> InvertedTable
insertFileToInvertedTable (Hash hash) filePath (InvertedTable table) = InvertedTable $ insert hash filePath table

--------------------------- Show Instances ------------------------------------

instance Show InvertedTable where
  show (InvertedTable trie) =
    mkString' "\n" (\(hash, path) -> show (Hash hash) ++ " -> " ++ path) (sortOn snd (toList trie))
