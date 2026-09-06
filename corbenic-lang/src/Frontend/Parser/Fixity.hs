module Frontend.Parser.Fixity (
    collectFixities,
) where

import Syntax.Fixity
import Syntax.Identifier
import Syntax.Location
import Syntax.Token

-- fetch all the fixity declarations out of the token stream
-- and assemble the fixity environment
collectFixities :: [Located Token] -> (FixityEnv, FixityEnv)
collectFixities toks =
    let (term, type') = foldMap lineFixities (splitLines toks)
     in (FixityEnv term, FixityEnv type')

-- split the token stream on newline
splitLines :: [Located Token] -> [[Located Token]]
splitLines [] = []
splitLines toks =
    let (line, rest) = break isNewline toks
     in line : splitLines (drop 1 rest)
  where
    isNewline (Annotated _ TokNewline) = True
    isNewline _ = False

-- get all fixities declared on a single line
lineFixities :: [Located Token] -> (Map Identifier Fixity, Map Identifier Fixity)
lineFixities line = foldMap oneFixity (findFixities line)

-- find all fixity declarators in a line, returning (fixity, operator, whether it's a type)
findFixities :: [Located Token] -> [(Fixity, Identifier, Bool)]
findFixities line = go line
  where
    go :: [Located Token] -> [(Fixity, Identifier, Bool)]
    go (Annotated _ (TokFixityDecl f) : rest) =
        case rest of
            -- infix operator shape (binary only): ⦿ f field operator
            (_ : Annotated _ (TokIdentifier op) : _)
                | isBinary f ->
                    (f, op, isInfixTypeOp (takeBefore op line)) : go (drop 2 rest)
            -- declaration shape: ⦿ f name
            (Annotated _ (TokIdentifier name) : _) ->
                (f, name, isTypeLine line) : go (drop 1 rest)
            _ -> go (drop 1 rest)
    go (_ : rest) = go rest
    go [] = []

-- is this fixity a binary operator
isBinary :: Fixity -> Bool
isBinary PrefixUnary = False
isBinary _ = True

-- get all the tokens before a given identifier
takeBefore :: Identifier -> [Located Token] -> [Located Token]
takeBefore ident = takeWhile (not . isTok)
  where
    isTok (Annotated _ (TokIdentifier i)) = i == ident
    isTok _ = False

-- decide whether an infix operator is a type operator (in a data glyph head, before the glyph)
-- or a term constructor (after the data glyph)
isInfixTypeOp :: [Located Token] -> Bool
isInfixTypeOp prefix =
    -- a type glyph before the operator means we are in the body (term)
    -- otherwise we are in the head (type).
    not (any isDataGlyph prefix)
  where
    isDataGlyph (Annotated _ t) = case t of
        TokData -> True
        TokNewtype -> True
        _ -> False

-- record one fixity into the term or type map.
oneFixity :: (Fixity, Identifier, Bool) -> (Map Identifier Fixity, Map Identifier Fixity)
oneFixity (f, name, isType)
    | isType = (mempty, one (name, f))
    | otherwise = (one (name, f), mempty)

-- whether a line declares a type (has a type declaration glyph).
isTypeLine :: [Located Token] -> Bool
isTypeLine = any isTypeGlyph
  where
    isTypeGlyph (Annotated _ t) = case t of
        TokTypeAlias -> True
        TokNewtype -> True
        TokData -> True
        TokConstraintAlias -> True
        TokTermDecl -> False
        TokTypeclassImplies -> False
        TokTypeclassInstance -> False
        TokModule -> False
        _ -> False
