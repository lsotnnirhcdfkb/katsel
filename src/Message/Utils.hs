module Message.Utils where

import qualified System.Console.ANSI as ANSI

makeIndentWithDivider :: Char -> String -> Int -> String
makeIndentWithDivider divider left indent = (replicate (indent - length left - 1) ' ') ++ left ++ " " ++ [divider] ++ " "

makeIndentStr :: Int -> String
makeIndentStr x = replicate x ' '

boldSGR :: ANSI.SGR
boldSGR = ANSI.SetConsoleIntensity ANSI.BoldIntensity

vividForeColorSGR :: ANSI.Color -> ANSI.SGR
vividForeColorSGR = ANSI.SetColor ANSI.Foreground ANSI.Vivid

filePathSGR :: [ANSI.SGR]
filePathSGR = [boldSGR, vividForeColorSGR ANSI.Cyan]

