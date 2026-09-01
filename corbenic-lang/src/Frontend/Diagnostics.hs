module Frontend.Diagnostics (
    Severity (..),
    Diagnostic (..),
    renderDiagnostic,
    Diagnosible (..)
) where

import Prelude

import Data.List qualified as List
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGRCode)

import Syntax.Location (Pos (..), Span (..))

data Severity
    = SevError
    | SevWarning
    deriving (Eq, Show)

data Diagnostic = Diagnostic
    { diagSeverity :: Severity
    , diagMessage :: Text
    , diagSpan :: Span
    }

class Diagnosible a where
  diagnose :: a -> Diagnostic

renderDiagnostic :: Diagnosible a => FilePath -> String -> a -> Text
renderDiagnostic file src diag =
    let (Diagnostic sev msg sp) = diagnose diag
        start = spanStart sp
        line = posLine start
        col = posCol start
     in color sev (label sev <> ": " <> msg)
            <> "\nat "
            <> toText file
            <> " "
            <> toText (show line <> ":" <> show col :: String)
            <> "\n"
            <> lineAt src line
            <> "\n"
            <> color sev (caretLine sp)
            <> "\n"

label :: Severity -> Text
label SevError = "error"
label SevWarning = "warning"

color :: Severity -> Text -> Text
color sev t = toText (setSGRCode [SetColor Foreground Vivid c]) <> t <> toText (setSGRCode [Reset])
  where
    c = case sev of
        SevError -> Red
        SevWarning -> Yellow

lineAt :: String -> Natural -> Text
lineAt src line =
    case drop (fromIntegral (line - 1)) (List.lines src) of
        (l : _) -> toText l
        [] -> ""

caretLine :: Span -> Text
caretLine (Span s e) =
    let startCol = posCol s
        endCol = posCol e
        pad = replicate (fromIntegral (startCol - 1)) ' '
        width = fromIntegral (endCol - startCol)
        mark = if width <= 1 then "^" else "^" <> replicate (width - 1) '-'
     in toText (pad <> mark)
