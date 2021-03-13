module Message where

import Location

data Section = SimpleText String

data SimpleDiagType = Error
                    | Warning

instance Show SimpleDiagType where
    show Error = "error"
    show Warning = "warning"

newtype DiagCode = DiagCode String

data SimpleDiag = SimpleDiag SimpleDiagType Location DiagCode String [Section]

report :: SimpleDiag -> String
report (SimpleDiag ty loc (DiagCode code) name sections) =
    header ++ "\n" ++
    shownSections ++
    footer ++ "\n"
    where
        header = show ty ++ " at " ++ asRowCol loc ++ ":"
        footer = indentStr ++ "==> [" ++ code ++ "]: " ++ name

        shownSections = concat $ map (showSection indentAmt) sections

        indentAmt = maximum $ map indentOf sections
        indentStr = makeIndentStr indentAmt

indentOf :: Section -> Int
indentOf _ = 4

showSection :: Int -> Section -> String
showSection indent (SimpleText text) = makeIndentStr indent ++ text ++ "\n"

makeIndentStr :: Int -> String
makeIndentStr x = replicate x ' '
