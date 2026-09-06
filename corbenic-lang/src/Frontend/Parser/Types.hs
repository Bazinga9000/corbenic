module Frontend.Parser.Types where

import Control.Monad.Writer (WriterT, tell)
import Text.Megaparsec hiding (ParseError, Token, many, some)

import Frontend.Diagnostics
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface
import Syntax.Token

type Parser = WriterT [ParseWarning] (Parsec Void [Located Token])

-- | Emit a parse warning.
warn :: ParseWarning -> Parser ()
warn = tell . one

data ParseError = ParseError Span ParseErrorKind

data ParseErrorKind
    = ParseUnexpectedEOF
    | ParseSyntaxError
    deriving (Eq, Show)

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

-- match a specific token and return the full located one
tok :: Token -> Parser (Located Token)
tok t = satisfy (\lt -> annVal lt == t)

-- match and void a token
tok_ :: Token -> Parser ()
tok_ = void . tok

-- a newline token
newline :: Parser ()
newline = tok_ TokNewline

-- an optional block beginning on the next line
optBlock :: Parser a -> Parser (Maybe a)
optBlock block = optional (try (newline *> block))

-- a surface name
name :: Parser (Located SurfaceName)
name = do
    Annotated sp t <- satisfy isNameIdent
    pure (Annotated sp (surfaceNameOf t))
  where
    isNameIdent (Annotated _ (TokIdentifier (IdentRaw _))) = True
    isNameIdent (Annotated _ (TokIdentifier (IdentQuoted _))) = True
    isNameIdent (Annotated _ (TokIdentifier (IdentPrimitive _))) = True
    isNameIdent _ = False

    surfaceNameOf :: Token -> SurfaceName
    surfaceNameOf (TokIdentifier (IdentRaw t)) = SNRaw t
    surfaceNameOf (TokIdentifier (IdentQuoted t)) = SNQuoted t
    surfaceNameOf (TokIdentifier (IdentPrimitive t)) = SNPrimitive t
    surfaceNameOf _ = error "surfaceNameOf: not a name"

-- | A module path: Name ('.' Name)*
modulePath :: Parser (ModulePath Span)
modulePath = do
    n <- name
    rest <- many (tok_ TokDot *> name)
    pure (n :| rest)
