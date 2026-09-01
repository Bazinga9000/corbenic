module Frontend.Lexer (
    scanTokens,
    LexError (..),
    LexErrorKind (..),
    LexWarning (..),
    LexWarningKind (..),
) where

import Prelude

import Frontend.Lexer.Tokenizers (lexOne)
import Frontend.Lexer.Types
import Syntax.Chars
import Syntax.Location
import Syntax.Token

-- | Lex a string into located tokens and warnings.
scanTokens :: String -> Either LexError ([Located Token], [LexWarning])
scanTokens src = runLexer walkLayout (LexState (src ++ "\n") 0 startPos [])
  where
    startPos = Pos 0 1 1

-- emits the indent/dedent tokens based on the 2 space rule, dispatches based on what line it is
walkLayout :: Lexer ()
walkLayout = do
    indent <- spanWhile isMidlineSpace
    let level = length indent
    mc <- peek
    case mc of
        Nothing -> do
            -- end of file, dedent as many times as needed
            lvl <- gets lexIndent
            replicateM_ lvl (emitHere TokDedent)
            emitHere TokEOF
        Just '\n' -> consume >> walkLayout -- empty line, do nothing
        Just c
            | isCommentStart c -> do
                isDoc <- isDocComment
                if isDoc
                    then do
                        -- eat the two comment chars
                        _ <- consume
                        _ <- consume
                        -- then the actual docs
                        doc <- breakWhile (== '\n')
                        emitHere (TokDocComment (stripLeadingSpace doc))
                        endLine >> walkLayout
                    else breakWhile (== '\n') >> walkLayout
            | otherwise -> do
                when (odd level) $
                    throwHere (LexInvalidIndentLevel (fromIntegral level))
                let newLevel = level `div` 2
                lvl <- gets lexIndent
                when (newLevel > lvl + 1) $
                    throwHere (LexLayoutJumped (fromIntegral newLevel))
                when (newLevel > lvl) $ emitHere TokIndent
                when (newLevel < lvl) $ replicateM_ (lvl - newLevel) (emitHere TokDedent)
                -- lex the line
                lexLine
                -- set indent level
                modify' (\st -> st{lexIndent = newLevel})
                -- emit newline and recurse
                endLine >> walkLayout

-- if we're at the end of the line, emit newline and eat the \n
endLine :: Lexer ()
endLine = do
    mc <- peek
    case mc of
        Just '\n' -> consume >> emitHere TokNewline
        _ -> pass

-- tokenize an entire line, consume everything but the newline
lexLine :: Lexer ()
lexLine = do
    mc <- peek
    case mc of
        Nothing -> pass
        Just '\n' -> pass
        Just c
            | isMidlineSpace c -> spanWhile isMidlineSpace >> lexLine
            | isCommentStart c -> do
                -- a mid-line ⍝⍝ is a warning; a single ⍝ is a trailing comment
                isDoc <- isDocComment
                when isDoc $ warn WarnMidlineDocComment
                void (breakWhile (== '\n'))
            | otherwise -> lexToken >> lexLine

-- lex the next token
lexToken :: Lexer ()
lexToken = do
    start <- gets lexPos
    mt <- lexOne
    case mt of
        Nothing -> pass
        Just t -> do
            end <- gets lexPos
            emit (Located (Span start end) t)

stripLeadingSpace :: String -> Text
stripLeadingSpace s = toText (dropWhile (== ' ') s)
