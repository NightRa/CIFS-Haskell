module Main where

import Lib
import Win32.ContextMenu

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

prog :: [String] -> IO ()
prog args = if length args /= 1
            then do
                  hPutStrLn stderr "The program needs a single argument - the file name."
                  end
            else let file = head args in do
              putStrLn $ "The SHA3 hash of the file '" ++ file ++ "' is: "
              hash <- hashFile file
              print $ BS.unpack hash
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
  prog args
