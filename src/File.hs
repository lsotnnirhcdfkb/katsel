module File
    ( File

    , file_name
    , file_source

    , open_file
    , make_file
    ) where

data File
    = File
      { file_name :: String
      , file_source :: String
      }
    | FakeFile
      { file_name :: String
      , file_source :: String
      }

instance Eq File where
    (==) f1 f2 = file_name f1 == file_name f2

open_file :: String -> IO File
open_file filename =
    readFile filename >>= \ file_contents ->
    return File
           { file_name = filename
           , file_source = file_contents
           }

make_file :: String -> String -> File
make_file name = FakeFile ("<" ++ name ++ ">")
