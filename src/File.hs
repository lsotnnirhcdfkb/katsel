module File where

data File = File
            { name :: String
            , source :: String
            }
            deriving Eq

open_file :: String -> IO File
open_file filename =
    readFile filename >>= \ file_contents ->
    return File
           { name = filename
           , source = file_contents
           }
