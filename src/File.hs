module File where

data File = File
            { name :: String
            , source :: String
            }

instance Eq File where
    (==) f1 f2 = name f1 == name f2

open_file :: String -> IO File
open_file filename =
    readFile filename >>= \ file_contents ->
    return File
           { name = filename
           , source = file_contents
           }
