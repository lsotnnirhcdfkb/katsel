module Colors
    ( file_path_sgr
    , error_sgr
    , warning_sgr
    , note_sgr
    , hint_sgr
    , dbgmsg_sgr
    , diagcode_sgr
    , diagname_sgr
    ) where

import qualified System.Console.ANSI as ANSI

bold_sgr :: [ANSI.SGR]
bold_sgr = [ANSI.SetConsoleIntensity ANSI.BoldIntensity]

vivid_fore_color_sgr :: ANSI.Color -> [ANSI.SGR]
vivid_fore_color_sgr = (:[]) . ANSI.SetColor ANSI.Foreground ANSI.Vivid

file_path_sgr, error_sgr, warning_sgr, note_sgr, hint_sgr, dbgmsg_sgr, diagcode_sgr, diagname_sgr :: [ANSI.SGR]

file_path_sgr = bold_sgr ++ vivid_fore_color_sgr ANSI.Cyan
error_sgr = bold_sgr ++ vivid_fore_color_sgr ANSI.Red
warning_sgr = bold_sgr ++ vivid_fore_color_sgr ANSI.Magenta
note_sgr = bold_sgr ++ vivid_fore_color_sgr ANSI.Green
hint_sgr = bold_sgr ++ vivid_fore_color_sgr ANSI.Blue
dbgmsg_sgr = note_sgr

diagcode_sgr = bold_sgr
diagname_sgr = bold_sgr
