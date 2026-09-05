module Frontend.Lexer.Tokenizers (
    lexOne,
    lexNumLiteral,
) where

import Prelude

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))

import Frontend.Lexer.Types
import Syntax.Chars
import Syntax.Fixity
import Syntax.Identifier
import Syntax.Literal
import Syntax.Token

-- | Lex a single token, or Nothing if we're at EOF
lexOne :: Lexer (Maybe Token)
lexOne = do
    start <- gets lexPos
    mc <- peek
    case mc of
        Nothing -> pure Nothing
        Just c
            | c == charQuote -> do
                _ <- consume
                ch <- consume
                q <- consume
                case (ch, q) of
                    (Just ch', Just q') | q' == charQuote -> pure (Just (TokLiteral (LitChar ch')))
                    _ -> throwAt start (LexUnexpected c)
            | c == charTextQuote -> do
                _ <- consume
                content <- breakWhile (== charTextQuote)
                q <- consume
                case q of
                    Just q' | q' == charTextQuote -> pure (Just (TokLiteral (LitText (toText content))))
                    _ -> throwAt start (LexUnexpected c)
            | Just q <- qBracketOf c -> Just <$> lexQ q
            | isDigit c -> do
                num <- spanWhile isDigitOrDot
                pure (Just (lexNumLiteral num))
            | c == charFixityDeclarator -> consume >> (Just <$> lexFixity)
            | isReserved c ->
                case Map.lookup c reservedTokens of
                    Just t -> consume >> pure (Just t)
                    Nothing -> throwAt start (LexUnexpected c)
            | Just cls <- identifierClass c
            , isUpperOf cls c -> do
                _ <- consume
                body <- spanWhile (isLetterOf cls)
                suffixes <- spanWhile isSuffixChar
                pure (Just (boolOrIdent (c : body ++ suffixes)))
            | isSuffixChar c -> throwAt start (LexUnexpected c)
            | otherwise -> do
                _ <- consume
                suffixes <- spanWhile isSuffixChar
                pure (Just (TokIdentifier (IdentRaw (toText (c : suffixes)))))

boolOrIdent :: String -> Token
boolOrIdent "True" = TokLiteral (LitBool True)
boolOrIdent "False" = TokLiteral (LitBool False)
boolOrIdent s = TokIdentifier (IdentRaw (toText s))

lexQ :: QBracket -> Lexer Token
lexQ q = do
    start <- gets lexPos
    _ <- consume
    content <- breakWhile (== qClose q)
    mc <- peek
    case mc of
        Just c
            | c == qClose q
            , not (any isReserved content)
            , '\n' `notElem` content ->
                consume >> pure (TokIdentifier (qIdent (toText content)))
        _ -> throwAt start (LexUnterminated q)
  where
    qIdent = case q of
        QPrimitive -> IdentPrimitive
        QTypeVar -> IdentType
        QGuillemet -> IdentQuoted

lexFixity :: Lexer Token
lexFixity = do
    start <- gets lexPos
    afterDecl <- gets lexInput
    mc <- peek
    case mc of
        Just c
            | c == charFixityUnary -> consume >> pure (TokFixityDecl PrefixUnary)
        _ -> do
            corners <- spanWhile (`elem` [charCornerL, charCornerR])
            digits <- spanWhile isSubDigit
            case (corners, unSubscript (toText digits)) of
                ([], Just prec)
                    | not (null digits) -> pure (TokFixityDecl (NonAssocBinary prec))
                ([corner], Just prec)
                    | not (null digits) -> pure (TokFixityDecl (fixityOf corner prec))
                _ -> throwAt start (LexBadFixity (toText fixityDecl)) where
                    fixityDecl = takeWhile (/= '\n') $ charFixityDeclarator : afterDecl
  where
    fixityOf c
        | c == charCornerL = LeftAssocBinary
        | c == charCornerR = RightAssocBinary
        | otherwise = NonAssocBinary

-- | Lex a numeric literal into either an natural or rational literal
lexNumLiteral :: String -> Token
lexNumLiteral s =
    let (natPart, fracWithDot) = break (== '.') s
        nat = readNatural natPart
     in case fracWithDot of
            [] -> TokLiteral (LitNatural nat)
            ('.' : fracPart)
                | all isDigit fracPart && not (null fracPart) ->
                    let num = toInteger nat * 10 ^ length fracPart + toInteger (readNatural fracPart)
                        den = 10 ^ length fracPart
                     in TokLiteral (LitRational (num % den))
                | otherwise -> error "lexNumLiteral: malformed decimal"
            _ -> error "lexNumLiteral: unreachable"
  where
    readNatural :: String -> Natural
    readNatural = foldl' step 0
      where
        step acc d
            | isDigit d = acc * 10 + fromIntegral (fromEnum d - fromEnum '0')
            | otherwise = error ("lexNumLiteral: non-digit in numeric literal: " <> show d)

isDigitOrDot :: Char -> Bool
isDigitOrDot c = isDigit c || c == '.'
