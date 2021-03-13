module Message
    ( Section(..)
    , SimpleDiagType(..)
    , SimpleDiag(..)
    , DiagCode(..)
    , ToDiagnostic
    , report
    , toDiagnostic
    ) where

import Location

import qualified System.Console.ANSI as ANSI

boldSGR :: ANSI.SGR
boldSGR = ANSI.SetConsoleIntensity ANSI.BoldIntensity

vividForeColorSGR :: ANSI.Color -> ANSI.SGR
vividForeColorSGR = ANSI.SetColor ANSI.Foreground ANSI.Vivid

filePathSGR :: [ANSI.SGR]
filePathSGR = [boldSGR, vividForeColorSGR ANSI.Cyan]

data Section = SimpleText String

data SimpleDiagType = Error
                    | Warning
                    | DebugMessage

instance Show SimpleDiagType where
    show Error = "error"
    show Warning = "warning"
    show DebugMessage = "debug message"

sgrOfDiagType :: SimpleDiagType -> [ANSI.SGR]
sgrOfDiagType Error = [boldSGR, vividForeColorSGR ANSI.Red]
sgrOfDiagType Warning = [boldSGR, vividForeColorSGR ANSI.Magenta]
sgrOfDiagType DebugMessage = [boldSGR, vividForeColorSGR ANSI.Green]

newtype DiagCode = DiagCode String

data SimpleDiag = SimpleDiag SimpleDiagType Span DiagCode String [Section]

class ToDiagnostic e where
    toDiagnostic :: e -> SimpleDiag

report :: (ToDiagnostic e) => e -> String
report = report' . toDiagnostic

report' :: SimpleDiag -> String
report' (SimpleDiag ty loc (DiagCode code) name sections) =
    header ++ "\n" ++
    shownSections ++
    footer ++ "\n"
    where
        header = ANSI.setSGRCode (sgrOfDiagType ty) ++ show ty ++ ANSI.setSGRCode [] ++ " at " ++ ANSI.setSGRCode filePathSGR ++ show loc ++ ANSI.setSGRCode [] ++ ":"
        footer = indentStr ++ "==> [" ++ ANSI.setSGRCode [boldSGR] ++ code ++ ANSI.setSGRCode [] ++ "]: " ++ name

        shownSections = concat $ map (showSection indentAmt) sections

        indentAmt = maximum $ map indentOf sections
        indentStr = makeIndentStr indentAmt

indentOf :: Section -> Int
indentOf _ = 4

showSection :: Int -> Section -> String
showSection indent (SimpleText text) = makeIndentStr indent ++ text ++ "\n"

makeIndentStr :: Int -> String
makeIndentStr x = take x $ repeat ' '
