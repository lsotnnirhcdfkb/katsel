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

import qualified Colors

data Section
    = SimpleText String
    | SimpleMultilineText String
    | Underlines UnderlinesSection
    | Note String

data SimpleDiagType
    = Error
    | Warning
    | DebugMessage
    | InternalError
    deriving Eq

text_of_diag_type :: SimpleDiagType -> String
text_of_diag_type Error = "error"
text_of_diag_type InternalError = "internal error!"
text_of_diag_type Warning = "warning"
text_of_diag_type DebugMessage = "debug message"

sgr_of_diag_type :: SimpleDiagType -> [ANSI.SGR]
sgr_of_diag_type Error = Colors.error_sgr
sgr_of_diag_type InternalError = Colors.error_sgr
sgr_of_diag_type Warning = Colors.warning_sgr
sgr_of_diag_type DebugMessage = Colors.dbgmsg_sgr

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
    footer ++ "\n"
    where
        header =
            ANSI.setSGRCode (sgr_of_diag_type ty) ++ text_of_diag_type ty ++ ANSI.setSGRCode [] ++
            (case maybe_span of
                Just sp -> " at " ++ ANSI.setSGRCode Colors.file_path_sgr ++ fmt_span sp ++ ANSI.setSGRCode []
                Nothing -> ""
            ) ++ ":"

        footer =
            case (maybe_diag_code, maybe_name) of
                (Just (DiagCode diag_code), Just diag_name) -> prefix ++ diag_code_fmt diag_code ++ ": " ++ diag_name ++ "\n"
                (Nothing                 , Just diag_name) -> prefix ++ diag_name ++ "\n"
                (Just (DiagCode diag_code), Nothing      ) -> prefix ++ diag_code_fmt diag_code ++ "\n"

                _ -> ""
            where
                diag_code_fmt code = "[" ++ ANSI.setSGRCode Colors.bold_sgr ++ code ++ ANSI.setSGRCode [] ++ "]"
                prefix = indent_str ++ "==> "

        shown_sections = concatMap (show_section indent_amt) sections

        indent_amt = maximum $ map indent_of sections
        indent_str = make_indent_str indent_amt

indent_of :: Section -> Int
indent_of (SimpleText _) = 4
indent_of (SimpleMultilineText _) = 4
indent_of (Underlines sec) = indent_of_underlines_section sec
indent_of (Note _) = 4

show_section :: Int -> Section -> String
show_section indent (SimpleText text) = make_indent_str indent ++ " " ++ text ++ "\n"
show_section indent (SimpleMultilineText text) = unlines $ map ((indent_str ++ " ")++) $ lines text
    where
        indent_str = make_indent_str indent
show_section indent (Note text) = make_indent_with_divider '\\' "" indent ++ notesgr ++ "note" ++ resetsgr ++ ": " ++ notesgr ++ text ++ resetsgr ++ "\n"
    where
        notesgr = ANSI.setSGRCode Colors.note_sgr
        resetsgr = ANSI.setSGRCode []
show_section indent (Underlines sec) = show_underlines_section indent sec
