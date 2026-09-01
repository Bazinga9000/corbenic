module Frontend.Lexer.Types where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.RWS.Strict hiding (pass)
import Data.List qualified as List

import Frontend.Diagnostics
import Syntax.Chars
import Syntax.Location
import Syntax.Token

data LexError = LexError Span LexErrorKind
    deriving (Eq, Show)

data LexErrorKind
    = LexUnexpected Char
    | LexLayoutJumped Natural
    | LexInvalidIndentLevel Natural
    | LexUnterminated QBracket
    | LexBadFixity Text
    deriving (Eq, Show)

instance Pretty LexErrorKind where
    prettyPrint (LexUnexpected c) = "unexpected character: " <> one c
    prettyPrint (LexLayoutJumped n) = "indentation jumped more than one level (to level " <> show n <> ")"
    prettyPrint (LexInvalidIndentLevel n) = "bad indentation level (" <> show n <> " is not a multiple of 2)"
    prettyPrint (LexUnterminated q) = "unterminated " <> bracketName q <> " bracket"
    prettyPrint (LexBadFixity t) = "malformed fixity declaration: " <> t

instance Pretty LexError where
    prettyPrint (LexError _ k) = prettyPrint k

instance Diagnosible LexError where
    diagnose (LexError s k) = Diagnostic
        { diagSeverity = SevError
        , diagMessage = prettyPrint k
        , diagSpan = s
        }

lexErrorSpan :: LexError -> Span
lexErrorSpan (LexError s _) = s

-- throw an error at the current position
throwHere :: LexErrorKind -> Lexer a
throwHere k = do
    p <- gets lexPos
    throwError (LexError (Span p p) k)

-- throw an error at some other position
throwAt :: Pos -> LexErrorKind -> Lexer a
throwAt p k = throwError (LexError (Span p p) k)


data LexWarning
    = LexWarning Span LexWarningKind
    deriving (Eq, Show)

data LexWarningKind
    = WarnMidlineDocComment
    deriving (Eq, Show)

instance Pretty LexWarningKind where
    prettyPrint WarnMidlineDocComment = "doc comment (⍝⍝) in the middle of a line is ignored"

instance Pretty LexWarning where
    prettyPrint (LexWarning _ k) = prettyPrint k

instance Diagnosible LexWarning where
    diagnose (LexWarning s k) = Diagnostic {
        diagSeverity = SevWarning,
        diagMessage = prettyPrint k,
        diagSpan = s
    }

-- | The lexer state. Note that tokens are stored in reverse (for easy prepending)
data LexState = LexState
    { lexInput :: String
    , lexIndent :: Int
    , lexPos :: Pos
    , lexOut :: [Located Token]
    }

-- | The lexer monad
type Lexer = RWST () [LexWarning] LexState (Except LexError)

-- | Run the lexer, returning the located tokens and warnings
runLexer :: Lexer () -> LexState -> Either LexError ([Located Token], [LexWarning])
runLexer m st = do
    (_, finalSt, warns) <- runExcept (runRWST m () st)
    pure (reverse (lexOut finalSt), warns)

-- get the current character
peek :: Lexer (Maybe Char)
peek = gets (viaNonEmpty head . lexInput)

-- eat the current character and emit nothing
consume :: Lexer (Maybe Char)
consume = do
    s <- gets lexInput
    case s of
        (c : rest) -> do
            modify' (\st -> st{lexInput = rest, lexPos = advance (lexPos st) c})
            pure (Just c)
        [] -> pure Nothing

-- get the longest prefix that always satisfies a predicate
spanWhile :: (Char -> Bool) -> Lexer String
spanWhile p = do
    s <- gets lexInput
    let (a, b) = span p s
    modify' (\st -> st{lexInput = b, lexPos = List.foldl' advance (lexPos st) a})
    pure a

-- get the longest prefix that does *not* satify a predicate
breakWhile :: (Char -> Bool) -> Lexer String
breakWhile p = spanWhile (not . p)

-- increment the position (either +1 char or +1 line)
advance :: Pos -> Char -> Pos
advance (Pos off line col) c
    | c == '\n' = Pos (off + 1) (line + 1) 1
    | otherwise = Pos (off + 1) line (col + 1)

-- emit a token
emit :: Located Token -> Lexer ()
emit t = modify' (\st -> st{lexOut = t : lexOut st})

-- emit a token with a zero-width span
emitHere :: Token -> Lexer ()
emitHere t = do
    p <- gets lexPos
    emit (Located (Span p p) t)

-- emit a warning, marked with the current position
warn :: LexWarningKind -> Lexer ()
warn k = do
    p <- gets lexPos
    tell [LexWarning (Span p p) k]

-- is this specifically a doc comment (begins with two comment chars)
isDocComment :: Lexer Bool
isDocComment = do
    s <- gets lexInput
    pure
        ( case s of
            ('⍝' : '⍝' : _) -> True
            _ -> False
        )
