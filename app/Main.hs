module Main where

import Types.Hash
import Types.Folder
import Types.Index
import IO.Hashing
import Win32.ContextMenu

import Demos.Demo2
import Demos.Demo3

import qualified Data.ByteString as BS
import System.Environment
import System.IO
import System.Exit

end :: IO ()
end = do
  putStrLn "Press any key to continue"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  _ <- getChar
  exitSuccess

prototype1_Hash :: FilePath -> IO ()
prototype1_Hash file = do
                         putStrLn $ "The SHA3 hash of the file '" ++ file ++ "' is: "
                         Hash hash <- hashFile file
                         print $ BS.unpack hash
                         end

prototype2_Index_Folder :: FilePath -> IO ()
prototype2_Index_Folder folder = do
                                  folder <- readFolderStructure folder
                                  print folder
                                  end

prototype3_Index_InvertedTable :: FilePath -> IO ()
prototype3_Index_InvertedTable folder = do
                                          index <- indexFolder folder
                                          print $ invertedTable index
                                          end

main :: IO ()
main = do
  -- putStrLn "Press enter to register in context menu: "
  -- _ <- getChar
  -- elevate
  -- addFileContextMenuItem "Hash" "Hash" (Just "C:/Users/Ilan/AppData/Roaming/local/bin/ProjectUnivIcon.ico") "\"C:/Users/Ilan/AppData/Roaming/local/bin/Hash.exe\" \"%1\""
  -- putStrLn "Enjoy!"
  -- end

  args <- getArgs
  if length args /= 1
  then do
       hPutStrLn stderr "The program needs a single argument - the file name."
       end
  else
    prototype3_Index_InvertedTable (head args)
