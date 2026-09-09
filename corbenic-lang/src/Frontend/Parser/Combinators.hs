module Frontend.Parser.Combinators where

import Text.Megaparsec hiding (ParseError, Token, many, some)

import Frontend.Parser.Types
import Syntax.Literal
import Syntax.Location
import Syntax.Surface
import Syntax.Token

-- match a specific token and return the full located one
tok :: Token -> Parser (Located Token)
tok t = satisfy (\lt -> annVal lt == t)

-- match and void a token
tok_ :: Token -> Parser ()
tok_ = void . tok

-- skip any fixity declaration, regardless of the fixity it carries
-- (used inside typeclass members, where the fixity itself is collected
-- separately into the FixityEnv before parsing)
tokFixity :: Parser ()
tokFixity = void $ satisfy $ \case
    (Annotated _ (TokFixityDecl _)) -> True
    _ -> False

-- get a literal
literal :: Parser (Located Literal)
literal = do
    (Annotated sp (TokLiteral lit)) <- satisfy isLiteral
    pure (Annotated sp lit)
  where
    isLiteral = \case
        (Annotated _ (TokLiteral _)) -> True
        _ -> False

-- a newline token
newline :: Parser ()
newline = tok_ TokNewline

-- an optional block beginning on the next line
optBlock :: Parser a -> Parser (Maybe a)
optBlock inner = optional (try (newline *> inner))

-- a thing, surrounded by indent and dedent
-- Every line is terminated by a newline, so the newline ending the preceeding
-- line must be consumed before the block's TokIndent.
block :: Parser a -> Parser (Span, Span, a)
block inner = do
    void (optional newline)
    open <- tok TokIndent
    a <- inner
    void (optional newline) -- an optional newline, to be robust regardless of whether the inner parser consumes it
    close <- tok TokDedent
    pure (spanOf open, spanOf close, a)

-- parse a thing possibly preceeded by the hidden declararation as a maybeexported thing
maybeExported :: (HasSpan (thing Span)) => Parser (thing Span) -> Parser (MaybeExported thing Span)
maybeExported parseThing = do
    hiddenTok <- optional (tok TokNoExport)
    theThing <- parseThing
    let thingSpan = spanOf theThing
    pure
        ( case hiddenTok of
            Just (Annotated tokSpan _) -> Hidden (tokSpan <> thingSpan) theThing
            Nothing -> Exported thingSpan theThing
        )

-- parse a list of things
listOf :: Parser a -> Parser (Span, Span, [a])
listOf p = do
    openBracket <- tok TokLBracket
    elems <- sepBy p (tok TokComma)
    closeBracket <- tok TokRBracket
    return $ (spanOf openBracket, spanOf closeBracket, elems)

-- parse a tuple of things
tupleOf :: Parser a -> Parser (Span, Span, NonEmpty a)
tupleOf p = do
    openParen <- tok TokLParen
    e1 <- p
    void $ tok TokComma
    elems <- sepBy p (tok TokComma)
    closeParen <- tok TokRParen
    return $ (spanOf openParen, spanOf closeParen, e1 :| elems)
