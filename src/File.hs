module File where

data File = File
            { name :: String
            , source :: String
            }

openFile :: String -> IO File
openFile filename =
    readFile filename >>= \ fileContents ->
    return File
    { name = filename
    , source = fileContents
    }
