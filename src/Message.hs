module Message
    ( Section(..)
    , SimpleDiagType(..)
    , SimpleDiag(..)
    , ToDiagnostic
    , to_diagnostic
    , report
    , make_code
    , make_indent_with_divider
    ) where

import Location

import qualified System.Console.ANSI as ANSI

import Message.Underlines(UnderlinesSection, show_underlines_section, indent_of_underlines_section)
import Message.Utils

data Section
    = SimpleText String
    | SimpleMultilineText String
    | Underlines UnderlinesSection

data SimpleDiagType
    = Error
    | Warning
    | DebugMessage
    | InternalError

text_of_diag_type :: SimpleDiagType -> String
text_of_diag_type Error = "error"
text_of_diag_type InternalError = "internal error!"
text_of_diag_type Warning = "warning"
text_of_diag_type DebugMessage = "debug message"

sgr_of_diag_type :: SimpleDiagType -> [ANSI.SGR]
sgr_of_diag_type Error = [bold_sgr, vivid_fore_color_sgr ANSI.Red]
sgr_of_diag_type InternalError = [bold_sgr, vivid_fore_color_sgr ANSI.Red]
sgr_of_diag_type Warning = [bold_sgr, vivid_fore_color_sgr ANSI.Magenta]
sgr_of_diag_type DebugMessage = [bold_sgr, vivid_fore_color_sgr ANSI.Green]

newtype DiagCode = DiagCode String

make_code :: String -> Maybe DiagCode
make_code str = Just $ DiagCode str

data SimpleDiag = SimpleDiag SimpleDiagType (Maybe Span) (Maybe DiagCode) (Maybe String) [Section]

class ToDiagnostic e where
    to_diagnostic :: e -> SimpleDiag
instance ToDiagnostic SimpleDiag where
    to_diagnostic = id

report :: (ToDiagnostic e) => e -> String
report = report' . to_diagnostic

report' :: SimpleDiag -> String
report' (SimpleDiag ty maybe_span maybe_diag_code maybe_name sections) =
    header ++ "\n" ++
    shown_sections ++
    footer
    where
        header =
            ANSI.setSGRCode (sgr_of_diag_type ty) ++ text_of_diag_type ty ++ ANSI.setSGRCode [] ++
            (case maybe_span of
                Just sp -> " at " ++ ANSI.setSGRCode file_path_sgr ++ fmt_span sp ++ ANSI.setSGRCode []
                Nothing -> ""
            ) ++ ":"

        footer =
            case (maybe_diag_code, maybe_name) of
                (Just (DiagCode diag_code), Just diag_name) -> prefix ++ diag_code_fmt diag_code ++ ": " ++ diag_name ++ "\n"
                (Nothing                 , Just diag_name) -> prefix ++ diag_name ++ "\n"
                (Just (DiagCode diag_code), Nothing      ) -> prefix ++ diag_code_fmt diag_code ++ "\n"

                _ -> ""
            where
                diag_code_fmt code = "[" ++ ANSI.setSGRCode [bold_sgr] ++ code ++ ANSI.setSGRCode [] ++ "]"
                prefix = indent_str ++ "==> "

        shown_sections = concatMap (show_section indent_amt) sections

        indent_amt = maximum $ map indent_of sections
        indent_str = make_indent_str indent_amt

indent_of :: Section -> Int
indent_of (SimpleText _) = 4
indent_of (SimpleMultilineText _) = 4
indent_of (Underlines sec) = indent_of_underlines_section sec

show_section :: Int -> Section -> String
show_section indent (SimpleText text) = make_indent_str indent ++ " " ++ text ++ "\n"
show_section indent (SimpleMultilineText text) = unlines $ map ((indent_str ++ " ")++) $ lines text
    where
        indent_str = make_indent_str indent
show_section indent (Underlines sec) = show_underlines_section indent sec
