module Message.Utils where

make_indent_with_divider :: Char -> String -> Int -> String
make_indent_with_divider divider left indent = replicate (indent - length left - 1) ' ' ++ left ++ " " ++ [divider] ++ " "

make_indent_str :: Int -> String
make_indent_str x = replicate x ' '
