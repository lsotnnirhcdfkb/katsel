module Message
    ( Section(..)
    , SimpleDiagType(..)
    , SimpleDiag(..)
    , ToDiagnostic
    , to_diagnostic
    , report
    , make_code
    ) where

import Location

import qualified System.Console.ANSI as ANSI

import Message.Underlines (Underline, show_underlines_section)
import Message.Arrows
import Message.Utils

import qualified Colors

data Section
    = SimpleText String
    | SimpleMultilineText String
    | Underlines [Underline]
    | Arrows [(Span, String)]
    | Note String

data SimpleDiagType
    = Error
    | Warning
    | DebugMessage
    | InternalError
    deriving (Eq, Ord)

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
make_code = Just . DiagCode

data SimpleDiag = SimpleDiag SimpleDiagType (Maybe Span) (Maybe DiagCode) (Maybe String) [Section]

class ToDiagnostic e where
    to_diagnostic :: e -> SimpleDiag
instance ToDiagnostic SimpleDiag where
    to_diagnostic = id

report :: (ToDiagnostic e) => e -> String
report = report' . to_diagnostic

report' :: SimpleDiag -> String
report' (SimpleDiag ty maybe_span maybe_diag_code maybe_name sections) =
    header ++
    shown_diag_lines ++
    footer ++
    "\n"
    where
        header =
            enclose_sgr (sgr_of_diag_type ty) (text_of_diag_type ty) ++
            (case maybe_span of
                Just sp -> " at " ++ enclose_sgr Colors.file_path_sgr (fmt_span sp)
                Nothing -> ""
            ) ++ ":\n"

        footer =
            case (maybe_diag_code, maybe_name) of
                (Just (DiagCode diag_code), Just diag_name) -> prefix ++ bracketify (enclose_sgr Colors.diagcode_sgr diag_code) ++ ": " ++ enclose_sgr Colors.diagname_sgr diag_name ++ "\n"
                (Nothing                  , Just diag_name) -> prefix ++ enclose_sgr Colors.diagname_sgr diag_name ++ "\n"
                (Just (DiagCode diag_code), Nothing       ) -> prefix ++ bracketify (enclose_sgr Colors.diagcode_sgr diag_code) ++ "\n"

                _ -> ""
            where
                bracketify = enclose "[" "]"
                prefix = replicate (indent_amt + 1) ' ' ++ "==> "

        diag_lines = concatMap show_section sections

        shown_diag_lines = concatMap show_diag_line diag_lines
            where
                show_diag_line (DiagLine prefix sep contents) = ind ++ prefix ++ " " ++ [sep] ++ " " ++ contents ++ "\n"
                    where
                        ind = replicate (indent_amt - length prefix) ' '

        indent_amt
            | null diag_lines = 0
            | otherwise = maximum $ map indent_of diag_lines
            where
                indent_of (DiagLine prefix _ _) = length prefix

show_section :: Section -> [DiagLine]
show_section (SimpleText text) = [DiagLine "" '=' text]
show_section (SimpleMultilineText text) = map (DiagLine "" '=') $ lines text
show_section (Note text) = [DiagLine "" '\\' $ enclose_sgr Colors.note_sgr "note" ++ ": " ++ enclose_sgr Colors.note_sgr text ++ "\n"]
show_section (Underlines us) = show_underlines_section us
show_section (Arrows places) = show_arrows places
