module Message
    ( Section(..)
    , SimpleDiagType(..)
    , SimpleDiag(..)
    , ToDiagnostic
    , toDiagnostic
    , report
    , makeCode
    , makeIndentWithDivider
    ) where

import Location

import qualified System.Console.ANSI as ANSI

import Message.Underlines(UnderlinesSection, showUnderlinesSection, indentOfUnderlinesSection)

boldSGR :: ANSI.SGR
boldSGR = ANSI.SetConsoleIntensity ANSI.BoldIntensity

vividForeColorSGR :: ANSI.Color -> ANSI.SGR
vividForeColorSGR = ANSI.SetColor ANSI.Foreground ANSI.Vivid

filePathSGR :: [ANSI.SGR]
filePathSGR = [boldSGR, vividForeColorSGR ANSI.Cyan]

data Section
    = SimpleText String
    | Underlines UnderlinesSection 

data SimpleDiagType
    = Error
    | Warning
    | DebugMessage

textOfDiagType :: SimpleDiagType -> String
textOfDiagType Error = "error"
textOfDiagType Warning = "warning"
textOfDiagType DebugMessage = "debug message"

sgrOfDiagType :: SimpleDiagType -> [ANSI.SGR]
sgrOfDiagType Error = [boldSGR, vividForeColorSGR ANSI.Red]
sgrOfDiagType Warning = [boldSGR, vividForeColorSGR ANSI.Magenta]
sgrOfDiagType DebugMessage = [boldSGR, vividForeColorSGR ANSI.Green]

newtype DiagCode = DiagCode String

makeCode :: String -> Maybe DiagCode
makeCode str = Just $ DiagCode str

data SimpleDiag = SimpleDiag SimpleDiagType (Maybe Span) (Maybe DiagCode) (Maybe String) [Section]

class ToDiagnostic e where
    toDiagnostic :: e -> SimpleDiag
instance ToDiagnostic SimpleDiag where
    toDiagnostic = id

report :: (ToDiagnostic e) => e -> String
report = report' . toDiagnostic

report' :: SimpleDiag -> String
report' (SimpleDiag ty maybeSpan maybeDiagCode maybeName sections) =
    header ++ "\n" ++
    shownSections ++
    footer
    where
        header =
            ANSI.setSGRCode (sgrOfDiagType ty) ++ textOfDiagType ty ++ ANSI.setSGRCode [] ++
            (case maybeSpan of
                Just sp -> " at " ++ ANSI.setSGRCode filePathSGR ++ fmtSpan sp ++ ANSI.setSGRCode []
                Nothing -> ""
            ) ++ ":"

        footer =
            case (maybeDiagCode, maybeName) of
                (Just (DiagCode diagCode), Just diagName) -> prefix ++ diagCodeFmt diagCode ++ ": " ++ diagName ++ "\n"
                (Nothing                 , Just diagName) -> prefix ++ diagName ++ "\n"
                (Just (DiagCode diagCode), Nothing      ) -> prefix ++ diagCodeFmt diagCode ++ "\n"

                _ -> ""
            where
                diagCodeFmt code = "[" ++ ANSI.setSGRCode [boldSGR] ++ code ++ ANSI.setSGRCode [] ++ "]"
                prefix = indentStr ++ "==> "

        shownSections = concat $ map (showSection indentAmt) sections

        indentAmt = maximum $ map indentOf sections
        indentStr = makeIndentStr indentAmt

indentOf :: Section -> Int
indentOf (SimpleText _) = 4
indentOf (Underlines sec) = indentOfUnderlinesSection sec

showSection :: Int -> Section -> String
showSection indent (SimpleText text) = makeIndentStr indent ++ " " ++ text ++ "\n"
showSection indent (Underlines sec) = showUnderlinesSection indent sec

makeIndentWithDivider :: Char -> String -> Int -> String
makeIndentWithDivider divider left indent = (replicate (indent - length left - 1) ' ') ++ left ++ " " ++ [divider] ++ " "

makeIndentStr :: Int -> String
makeIndentStr x = replicate x ' '
