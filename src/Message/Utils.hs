module Message.Utils where

import qualified System.Console.ANSI as ANSI

make_indent_with_divider :: Char -> String -> Int -> String
make_indent_with_divider divider left indent = replicate (indent - length left - 1) ' ' ++ left ++ " " ++ [divider] ++ " "

make_indent_str :: Int -> String
make_indent_str x = replicate x ' '

bold_sgr :: ANSI.SGR
bold_sgr = ANSI.SetConsoleIntensity ANSI.BoldIntensity

vivid_fore_color_sgr :: ANSI.Color -> ANSI.SGR
vivid_fore_color_sgr = ANSI.SetColor ANSI.Foreground ANSI.Vivid

file_path_sgr :: [ANSI.SGR]
file_path_sgr = [bold_sgr, vivid_fore_color_sgr ANSI.Cyan]

