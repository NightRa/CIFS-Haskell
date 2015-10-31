{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Win32.ContextMenu (getExecutableName,
                          addFileContextMenuItem,
                          elevate) where

import System.Win32.DLL (getModuleFileName)
import System.Win32.Registry
import System.Win32.Types
import Foreign.Ptr
import Foreign.C.Types

getExecutableName :: IO String
getExecutableName = getModuleFileName nullPtr

-- | For all files, on right click, display 'menuName' with the optinal 'icon' to execute the 'command'.
addFileContextMenuItem :: String -> String -> Maybe FilePath -> String -> IO ()
addFileContextMenuItem regKeyName menuName icon command = do
  key <- regCreateKey hKEY_CLASSES_ROOT ("*\\shell\\" ++ regKeyName)
  regSetStringValue key "" menuName -- "" for default value name.
  case icon of
    Just iconPath -> regSetStringValue key "Icon" iconPath
    Nothing -> return ()
  keyCommand <- regCreateKey key "command"
  regSetStringValue keyCommand "" command -- "" for default value name.

  regCloseKey key
  regCloseKey keyCommand

elevate :: IO ()
elevate = do
  exeName <- getExecutableName
  _ <- withTString "runas" $ \c_runAs ->
        withTString exeName $ \c_exeName ->
          c_ShellExecute nullPtr c_runAs c_exeName nullPtr nullPtr 1
  return ()
foreign import ccall unsafe "Shellapi.h ShellExecuteW"
  c_ShellExecute :: Ptr () -> LPCTSTR -> LPCTSTR -> LPCTSTR -> LPCTSTR -> CInt -> IO ErrCode
