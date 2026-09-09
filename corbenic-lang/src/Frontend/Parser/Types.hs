module Frontend.Parser.Types where

import Control.Monad.RWS.Strict (RWST, tell)
import Text.Megaparsec hiding (ParseError, Token, many, some)

import Frontend.Diagnostics
import Syntax.Fixity
import Syntax.Location
import Syntax.Token

-- parser monad
type Parser = RWST (FixityEnv, FixityEnv) [ParseWarning] () (Parsec Void [Located Token])

warn :: ParseWarning -> Parser ()
warn = tell . one

termFixity :: Parser FixityEnv
termFixity = asks fst

typeFixity :: Parser FixityEnv
typeFixity = asks snd

data ParseError = ParseError Span ParseErrorKind
    deriving (Eq, Ord, Show)

data ParseErrorKind
    = ParseUnexpectedEOF
    | ParseSyntaxError
    deriving (Eq, Ord, Show)

instance Pretty ParseErrorKind where
    prettyPrint ParseUnexpectedEOF = "unexpected end of input"
    prettyPrint ParseSyntaxError = "syntax error"

instance Pretty ParseError where
    prettyPrint (ParseError _ k) = prettyPrint k

instance Diagnosible ParseError where
    diagnose (ParseError s k) =
        Diagnostic
            { diagSeverity = SevError
            , diagMessage = prettyPrint k
            , diagSpan = s
            }

data ParseWarning = ParseWarning Span ParseWarningKind

data ParseWarningKind
    = WarnMixedImport
    deriving (Eq, Show)

instance Pretty ParseWarningKind where
    prettyPrint WarnMixedImport = "mixing selected and hidden imports is redundant"

instance Pretty ParseWarning where
    prettyPrint (ParseWarning _ k) = prettyPrint k

instance Diagnosible ParseWarning where
    diagnose (ParseWarning s k) =
        Diagnostic
            { diagSeverity = SevWarning
            , diagMessage = prettyPrint k
            , diagSpan = s
            }
