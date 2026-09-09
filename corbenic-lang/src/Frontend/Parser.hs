module Frontend.Parser (
    parseModule,
    parseSurfaceExpr,
    Frontend.Parser.parse,
) where

import Control.Monad.RWS.Strict (runRWST)
import Text.Megaparsec hiding (ParseError, Token, many, some)

import Control.Monad.Combinators.Expr
import Frontend.Parser.Combinators
import Frontend.Parser.Fixity (collectFixities)
import Frontend.Parser.Types
import Data.Map qualified as M
import Data.List qualified as L
import Syntax.Documentation
import Syntax.Fixity
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface
import Syntax.Token
import Relude.Extra (foldl1')

-- an identifier (will allow primitives, illegally placed primitives are rejected by TC)
parseIdentifier :: Parser (Located Identifier)
parseIdentifier = do
    Annotated sp t <- satisfy isIdentifier
    pure (Annotated sp (identifierOf t))
  where
    isIdentifier (Annotated _ (TokIdentifier _)) = True
    isIdentifier _ = False

    identifierOf :: Token -> Identifier
    identifierOf (TokIdentifier i) = i
    identifierOf _ = error "parseIdentifier: not an identifier"

---------------------------------------------
-- MODULE PARSERS
---------------------------------------------

-- full module
parseModule :: Parser (SurfaceModule Span)
parseModule = do
    termFix <- termFixity
    typeFix <- typeFixity
    dc <- optional parseDocComment
    tok_ TokModule
    nm <- parseModulePath
    newline
    imports <- parseImportList
    void (many newline)
    decls <- many (maybeExported parseSurfaceDeclaration)
    void (many newline)
    tok_ TokEOF
    pure SurfaceModule{smName = nm, smDoc = dc, smImports = imports, smDecls = decls, smTermFixity = termFix, smTypeFixity = typeFix}

-- a module path, `.` separated names
parseModulePath :: Parser (ModulePath Span)
parseModulePath = do
    n <- parseIdentifier
    rest <- many (tok_ TokDot *> parseIdentifier)
    pure (n :| rest)

-- a qualifier, ⌸ or ⌸ Name
parseQualifier :: Parser Qualifier
parseQualifier = do
    tok_ TokQualify
    mn <- optional parseIdentifier
    pure (maybe ImplicitQualifier (ExpliitQualifier . annVal) mn)

-- an import name: Name or ※ Name
parseImportName :: Parser (Either (Located Identifier) (Located Identifier))
parseImportName = (Left <$> parseIdentifier) <|> (Right <$> (tok_ TokNoExport *> parseIdentifier))

-- a block of import names
parseImportBlock :: Parser (Span, ImportSpec Span)
parseImportBlock = do
    (openSp, closeSp, names) <- block (many (parseImportName <* newline))
    let (sel, excl) = partitionEithers names
        sp = openSp <> closeSp
    when (not (null sel) && not (null excl)) $
        warn (ParseWarning sp WarnMixedImport)
    pure (sp, ImportSelect sel excl)

-- an import: ⇲ ModulePath (qualifier)? (newline block)?
parseImportDecl :: Parser (SurfaceImport Span)
parseImportDecl = do
    glyph <- tok TokImport
    path <- parseModulePath
    q <- optional parseQualifier
    newline
    mblock <- optional parseImportBlock
    let end = maybe (spanOf (last path)) fst mblock
    pure (SurfaceImport (spanOf glyph <> end) path q (snd <$> mblock))

-- a re-export ⇱ ModulePath, then a newline, then an optional block.
parseReexportDecl :: Parser (SurfaceImport Span)
parseReexportDecl = do
    glyph <- tok TokReexport
    path <- parseModulePath
    newline
    mblock <- optional parseImportBlock
    let end = maybe (spanOf (last path)) fst mblock
        spec = maybe ImportAll snd mblock
    pure (SurfaceReexport (spanOf glyph <> end) path spec)

-- the imports and re-exports of a module, one per line.
parseImportList :: Parser [SurfaceImport Span]
parseImportList = many (parseImportDecl <|> parseReexportDecl)

-- a contiguous stream of doc comments, merged
parseDocComment :: Parser DocComment
parseDocComment = do
    Just docs <- nonEmpty <$> some (satisfy isDocComment)
    let totalSpan = combineAll docs
        fullText = toList $ fmap getDocLine docs
    pure (DocComment totalSpan fullText)
  where
    isDocComment (Annotated _ (TokDocComment _)) = True
    isDocComment _ = False
    getDocLine (Annotated _ (TokDocComment ln)) = ln
    getDocLine _ = error "parseDocComment: not a doc comment"

---------------------------------------------
-- EXPRESSION PARSERS
---------------------------------------------

-- top level expr parser
parseSurfaceExpr :: Parser (SurfaceExpr Span)
parseSurfaceExpr = do
    e <- parseExprPratt
    mwhere <- optional (try parseWhereBlock)
    pure (maybe e (mkWhere e) mwhere)
  where
    mkWhere :: SurfaceExpr Span -> [SurfaceWhereDeclaration Span] -> SurfaceExpr Span
    mkWhere e wheres = SEWhere (combine e (spanOf (L.last wheres))) e wheres

-- the infix/unary pratt level for terms
parseExprPratt :: Parser (SurfaceExpr Span)
parseExprPratt = do
    fixity <- termFixity
    let table = mkTermOperatorTable fixity
    if null table
        then parseApplication
        else makeExprParser parseApplication table

parseApplication :: Parser (SurfaceExpr Span)
parseApplication = do
    e <- parseOperand
    rest <- many parseOperand
    pure (foldl' mkApp e rest)
  where
    mkApp :: SurfaceExpr Span -> SurfaceExpr Span -> SurfaceExpr Span
    mkApp f x = SEApp (combine f x) f x

-- a simple (non-type-app) operand followed by some type variables
parseOperand :: Parser (SurfaceExpr Span)
parseOperand = do
    e <- parseSimpleOperand
    many_ e
  where
    -- fold type-application suffixes onto the accumulated expression
    many_ acc = do
        next <- optional (typeAppSuffix acc)
        case next of
            Nothing -> pure acc
            Just acc' -> many_ acc'

-- a simple operand: an atom that is not a type application.
parseSimpleOperand :: Parser (SurfaceExpr Span)
parseSimpleOperand =
    parseSELiteral
        <|> parseSEIdentifier
        <|> try parseSELambda
        <|> parseSELambdaCase
        <|> parseSETypeLambda
        <|> parseSEDo
        <|> parseSECase
        <|> parseSEList
        <|> try parseSETuple
        <|> try parseSEOpSectionL
        <|> try parseSEOpSectionR
        <|> try parseSEAnnotation
        <|> parseSEHole
        <|> (tok TokLParen *> parseSurfaceExpr <* tok TokRParen)

-- one type-application suffix: 〈type〉
typeAppSuffix :: SurfaceExpr Span -> Parser (SurfaceExpr Span)
typeAppSuffix e = do
    langle <- tok TokAngleL
    ty <- parseSurfaceType
    rangle <- tok TokAngleR
    pure (SETypeApp (combine4 e langle ty rangle) e ty)

-- a single literal token
parseSELiteral :: Parser (SurfaceExpr Span)
parseSELiteral = SELiteral <$> literal

-- an identifier used as an expression
parseSEIdentifier :: Parser (SurfaceExpr Span)
parseSEIdentifier = do
    fixity <- termFixity
    let FixityEnv m = fixity
    Annotated sp t <- satisfy (\lt -> case annVal lt of
        TokIdentifier (IdentRaw i) -> M.lookup (IdentRaw i) m == Nothing -- raw are only allowed if they aren't fixity'd
        TokIdentifier _ -> True -- quoted/primitives are always usable as values
        _ -> False)
    pure (SEIdentifier (Annotated sp (identifierOf t)))
  where
    identifierOf :: Token -> Identifier
    identifierOf (TokIdentifier i) = i
    identifierOf _ = error "parseSEIdentifier: not an identifier"

-- a term lambda: λx ↦ body
parseSELambda :: Parser (SurfaceExpr Span)
parseSELambda = do
    lam <- tok TokLambda
    ident <- parseIdentifier
    mapsTo <- tok TokMapsTo
    body <- parseSurfaceExpr
    pure (SELambda (combine4 lam ident mapsTo body) ident body)

-- a type lambda: Λa ↦ body
parseSETypeLambda :: Parser (SurfaceExpr Span)
parseSETypeLambda = do
    lam <- tok TokTypeLambda
    ident <- parseIdentifier
    mapsTo <- tok TokMapsTo
    body <- parseSurfaceExpr
    pure (SETypeLambda (combine4 lam ident mapsTo body) ident body)

-- a monadic do block
parseSEDo :: Parser (SurfaceExpr Span)
parseSEDo = do
    doTok <- tok TokDo
    (openSpan, closeSpan, doInstrs) <- block $ parseDoBlock
    return $ SEDo (combine3 doTok openSpan closeSpan) doInstrs
  where
    parseDoBlock :: Parser (NonEmpty (SurfaceDoInstruction Span))
    parseDoBlock = do
        Just stmts <- nonEmpty <$> some (statement <* newline)
        pure stmts
    -- one statement: a bind or a bare monadic statement.
    statement :: Parser (SurfaceDoInstruction Span)
    statement =
        try bindInstr <|> (SDIMonadicStmt <$> parseSurfaceExpr)
    -- a do block's bind: `x ≔ e` or `x ↤ e`.
    bindInstr :: Parser (SurfaceDoInstruction Span)
    bindInstr =
        try (mkDoInstr TokTermDecl SDIBindName)
            <|> mkDoInstr TokDoMonadic SDIExtractMonad

    mkDoInstr :: Token -> (Span -> Annotated Span Identifier -> SurfaceExpr Span -> SurfaceDoInstruction Span) -> Parser (SurfaceDoInstruction Span)
    mkDoInstr declarator constructor = do
        binding <- parseIdentifier
        decl <- tok declarator
        body <- parseSurfaceExpr
        return $ constructor (combine3 binding decl body) binding body

-- a pattern match
parseSECase :: Parser (SurfaceExpr Span)
parseSECase = do
    caseTok <- tok TokCase
    scrutinee <- parseSurfaceExpr
    (openSpan, closeSpan, patterns) <- block $ many (parseSurfaceBranch <* newline)
    return $ SECase (combine4 caseTok scrutinee openSpan closeSpan) scrutinee patterns

-- a pattern match automatically wrapped in a lambda
parseSELambdaCase :: Parser (SurfaceExpr Span)
parseSELambdaCase = do
    lambdaTok <- tok TokLambda
    caseTok <- tok TokCase
    (openSpan, closeSpan, patterns) <- block $ many (parseSurfaceBranch <* newline)
    return $ SELambdaCase (combine4 lambdaTok caseTok openSpan closeSpan) patterns

-- a list literal
parseSEList :: Parser (SurfaceExpr Span)
parseSEList = do
    (open, close, elems) <- listOf parseSurfaceExpr
    return $ SEList (combine open close) elems

-- a tuple literal (must be nonempty)
parseSETuple :: Parser (SurfaceExpr Span)
parseSETuple = do
    (open, close, elems) <- tupleOf parseSurfaceExpr
    return $ SETuple (combine open close) elems

-- (3+)
parseSEOpSectionL :: Parser (SurfaceExpr Span)
parseSEOpSectionL = do
    lParen <- tok TokLParen
    e <- parseOperand
    op <- parseInfixOp
    rParen <- tok TokRParen
    pure (SEOpSectionL (combine4 lParen e op rParen) e op)

-- (+3)
parseSEOpSectionR :: Parser (SurfaceExpr Span)
parseSEOpSectionR = do
    lParen <- tok TokLParen
    op <- parseInfixOp
    e <- parseOperand
    rParen <- tok TokRParen
    pure (SEOpSectionR (combine4 lParen op e rParen) op e)

-- a single infix operator (a binary operator from the term fixity env).
parseInfixOp :: Parser (Annotated Span Identifier)
parseInfixOp = do
    fixity <- termFixity
    let FixityEnv m = fixity
        binaryOps = [ ident | (ident, f) <- M.assocs m, isBinary f ]
    t <- satisfy (\lt -> case annVal lt of
        TokIdentifier i -> any (\op -> sameIdent op i) binaryOps
        _ -> False)
    pure (Annotated (spanOf t) (identOf (annVal t)))
  where
    identOf :: Token -> Identifier
    identOf (TokIdentifier i) = i
    identOf _ = error "parseInfixOp: not an identifier"

-- an inline type annotation
parseSEAnnotation :: Parser (SurfaceExpr Span)
parseSEAnnotation = do
    lParen <- tok TokLParen
    e <- parseSurfaceExpr
    hasType <- tok TokHasType
    ty <- parseSurfaceType
    rParen <- tok TokRParen
    return $ SEAnnotation (combine5 lParen e hasType ty rParen) e ty

-- a hole _
parseSEHole :: Parser (SurfaceExpr Span)
parseSEHole = SEHole . spanOf <$> tok TokHole

---------------------------------------------
-- PATTERN PARSERS
---------------------------------------------

parseSurfaceBranch :: Parser (SurfaceBranch Span)
parseSurfaceBranch = do
    pat <- parseSurfacePattern
    mapsTo <- tok TokMapsTo
    body <- parseSurfaceExpr
    return $ SurfaceBranch (combine3 pat mapsTo body) pat body

parseSurfacePattern :: Parser (SurfacePattern Span)
parseSurfacePattern = do
    h <- parsePatternAtom
    args <- many parsePatternAtom
    case args of
        [] -> pure h
        _ -> case h of
            SPVar (Annotated sp ident) -> pure (SPCon sp (Annotated sp ident) args)
            _ -> error "parseSurfacePattern: application needs a constructor head"


parsePatternAtom :: Parser (SurfacePattern Span)
parsePatternAtom =
    parseSPLiteral
        <|> parseSPVar
        <|> parseSPTuple
        <|> parseSPList
        <|> parseSPWild

parseSPLiteral :: Parser (SurfacePattern Span)
parseSPLiteral = SPLiteral <$> literal

parseSPVar :: Parser (SurfacePattern Span)
parseSPVar = SPVar <$> parseIdentifier

parseSPTuple :: Parser (SurfacePattern Span)
parseSPTuple = do
    (open, close, elems) <- tupleOf parsePatternAtom
    return $ SPTuple (combine open close) elems

parseSPList :: Parser (SurfacePattern Span)
parseSPList = do
    (open, close, elems) <- listOf parsePatternAtom
    return $ SPList (combine open close) elems

parseSPWild :: Parser (SurfacePattern Span)
parseSPWild = SPWild . annTag <$> tok TokHole

---------------------------------------------
-- TYPE PARSERS
---------------------------------------------

parseSurfaceType :: Parser (SurfaceType Span)
parseSurfaceType = do
    -- ⇒ binds looser than everything else, check it here
    mctx <- optional (try (parseSurfaceClassContext <* tok_ TokTypeclassImplies))
    body <- parseTypeBody
    case mctx of
        Nothing -> pure body
        Just ctx -> pure (STConstrained (combine ctx body) ctx body)
  where
    parseTypeBody :: Parser (SurfaceType Span)
    parseTypeBody = do
        fixity <- typeFixity
        let table = mkTypeOperatorTable fixity
        if null table
            then parseSTApp
            else makeExprParser parseSTApp table

-- a type application: one or more type operands applied left-associatively.
parseSTApp :: Parser (SurfaceType Span)
parseSTApp = do
    e <- parseSTOperand
    rest <- many parseSTOperand
    pure (foldl' mkApp e rest)
  where
    mkApp :: SurfaceType Span -> SurfaceType Span -> SurfaceType Span
    mkApp f x = STApp (combine f x) f x

parseSTOperand :: Parser (SurfaceType Span)
parseSTOperand = parseSTAtom <|> try parseSTConstraint

-- an atomic type, excluding constraints.
parseSTAtom :: Parser (SurfaceType Span)
parseSTAtom =
    parseSTName
        <|> parseSTList
        <|> try parseSTTuple
        <|> try parseSTForall
        <|> try parseSTExists

parseSTName :: Parser (SurfaceType Span)
parseSTName = STName <$> parseIdentifier

parseSTList :: Parser (SurfaceType Span)
parseSTList = do
    lb <- tok TokLBracket
    ty <- parseSurfaceType
    rb <- tok TokRBracket
    return $ STList (combine3 lb ty rb) ty

parseSTTuple :: Parser (SurfaceType Span)
parseSTTuple = do
    (open, close, elems) <- tupleOf parseSurfaceType
    return $ STTuple (combine open close) elems

parseSTConstraint :: Parser (SurfaceType Span)
parseSTConstraint = (\x -> STConstraint (spanOf x) x) <$> parseSurfaceClassContext

parseSTForall :: Parser (SurfaceType Span)
parseSTForall = do
    forAll <- tok TokForAll
    Just qualified <- nonEmpty <$> some parseIdentifier
    dot <- tok TokDot
    ty <- parseSurfaceType
    return $ STForall (combine3 forAll dot ty) qualified ty

parseSTExists :: Parser (SurfaceType Span)
parseSTExists = do
    forAll <- tok TokExists
    Just qualified <- nonEmpty <$> some parseIdentifier
    dot <- tok TokDot
    ty <- parseSurfaceType
    return $ STExists (combine3 forAll dot ty) qualified ty

---------------------------------------------
-- DECLARATION PARSERS
---------------------------------------------

parseSurfaceTermDecl :: Parser (SurfaceTermDecl Span)
parseSurfaceTermDecl = do
    dc <- optional parseDocComment
    _ <- optional tokFixity -- a binding may be prefixed by its fixity declarator
    name <- parseIdentifier
    declarator <- tok TokTermDecl
    body <- parseSurfaceExpr
    void (optional newline) -- a block body already consumed the newline before its dedent
    return $ SurfaceTermDecl (combine3 name declarator body) dc name body

parseSurfaceTypeDecl :: Parser (SurfaceTypeDecl Span)
parseSurfaceTypeDecl = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    declarator <- tok TokHasType
    ty <- parseSurfaceType
    newline
    return $ SurfaceTypeDecl (combine3 name declarator ty) dc name ty

parseSurfaceDataDecl :: Parser (SurfaceDataDecl Span)
parseSurfaceDataDecl = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    params <- many parseIdentifier
    declarator <- tok TokData
    ctors <- parseDataConstructors
    let totalSpan = combineAll $ spanOf name :| fmap spanOf params <> [spanOf declarator]
    return $ SurfaceDataDecl totalSpan dc name params ctors
  where
    parseDataConstructors :: Parser [MaybeExported SurfaceTypeConstructor Span]
    parseDataConstructors = do
        inline <- sepBy (maybeExported parseSurfaceTypeConstructor) (tok TokConstructorBar)
        mblock <- optional (try (do
            (_, _, ctors) <- block (many (tok_ TokConstructorBar *> maybeExported parseSurfaceTypeConstructor <* newline))
            pure ctors))
        pure (inline <> maybe [] id mblock)

parseSurfaceNewtypeDecl :: Parser (SurfaceNewtypeDecl Span)
parseSurfaceNewtypeDecl = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    params <- many parseIdentifier
    declarator <- tok TokNewtype
    ctor <- maybeExported parseSurfaceTypeConstructor
    let totalSpan = combineAll $ spanOf name :| fmap spanOf params <> [spanOf declarator]
    return $ SurfaceNewtypeDecl totalSpan dc name params ctor

parseSurfaceTypeAlias :: Parser (SurfaceTypeAlias Span)
parseSurfaceTypeAlias = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    params <- many parseIdentifier
    declarator <- tok TokTypeAlias
    ty <- parseSurfaceType
    return $ SurfaceTypeAlias (combine3 name declarator ty) dc name params ty

parseSurfaceConstraintAlias :: Parser (SurfaceConstraintAlias Span)
parseSurfaceConstraintAlias = do
    dc <- optional parseDocComment
    cstHead <- parseSurfaceClassApp
    declarator <- tok TokConstraintAlias
    cstBody <- parseSurfaceClassContext
    return $ SurfaceConstraintAlias (combine3 cstHead declarator cstBody) dc cstHead cstBody

parseSurfaceAssociatedType :: Parser (SurfaceAssociatedType Span)
parseSurfaceAssociatedType = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    params <- many parseIdentifier
    declarator <- tok TokAssociatedType
    ty <- optional parseSurfaceType
    void (optional newline)
    let totalSpan = combineAll $ spanOf name :| fmap spanOf params <> maybe [] one (fmap spanOf ty) <> [spanOf declarator]
    return $ SurfaceAssociatedType totalSpan dc name params ty

parseSurfaceTypeConstructor :: Parser (SurfaceTypeConstructor Span)
parseSurfaceTypeConstructor = do
    dc <- optional parseDocComment
    name <- parseIdentifier
    fields <- many parseSurfaceType
    let totalSpan = combineAll $ spanOf name :| fmap spanOf fields
    pure (SurfaceTypeConstructor totalSpan dc name fields)

parseSurfaceClassMember :: Parser (SurfaceClassMember Span)
parseSurfaceClassMember = try method <|> (SCAssociatedType <$> parseSurfaceAssociatedType) where
    method = do
        _ <- optional tokFixity -- we might have a fixity declaration, ignore it
        sig <- parseSurfaceTypeDecl
        defTerm <- optional (try parseSurfaceTermDecl)
        pure (SCMethod sig defTerm)

parseWhereBlock :: Parser [SurfaceWhereDeclaration Span]
parseWhereBlock = do
    (_, _, decls) <- block (many parseWhereDecl)
    pure decls

parseWhereDecl :: Parser (SurfaceWhereDeclaration Span)
parseWhereDecl = do
    mty <- optional (try parseSurfaceTypeDecl)
    term <- parseSurfaceTermDecl
    pure (SWDTerm mty term)

parseSurfaceInstanceMember :: Parser (SurfaceInstanceMember Span)
parseSurfaceInstanceMember =
    (try (SIMMethod <$> parseSurfaceTermDecl)) <|> (SIMAssociatedType <$> parseSurfaceAssociatedType)

parseSurfaceClassDecl :: Parser (SurfaceClassDecl Span)
parseSurfaceClassDecl = do
    dc <- optional parseDocComment
    -- either `(context) ⇒ Head` or bare `⇒ Head`.
    msuper <- optional (try parseSurfaceClassContext)
    implies <- tok TokTypeclassImplies
    headDecl <- parseSurfaceClassApp
    (_, closeSp, members) <- block (many parseSurfaceClassMember)
    return $ SurfaceClassDecl (combine3 headDecl implies closeSp) dc msuper headDecl members

parseSurfaceInstanceDecl :: Parser (SurfaceInstanceDecl Span)
parseSurfaceInstanceDecl = do
    dc <- optional parseDocComment
    tok_ TokTypeclassInstance -- ∴
    headDecl <- parseSurfaceClassApp
    (_, closeSp, members) <- block (many parseSurfaceInstanceMember)
    return $ SurfaceInstanceDecl (combine (spanOf headDecl) closeSp) dc headDecl members

parseSurfaceDeclaration :: Parser (SurfaceDeclaration Span)
parseSurfaceDeclaration =
    try (SDTerm <$> (Just <$> parseSurfaceTypeDecl) <*> parseSurfaceTermDecl)
        <|> try (SDTerm Nothing <$> parseSurfaceTermDecl)
        <|> try (SDData <$> parseSurfaceDataDecl)
        <|> try (SDNewtype <$> parseSurfaceNewtypeDecl)
        <|> try (SDTypeAlias <$> parseSurfaceTypeAlias)
        <|> try (SDConstraintAlias <$> parseSurfaceConstraintAlias)
        <|> try (SDClass <$> parseSurfaceClassDecl)
        <|> (SDInstance <$> parseSurfaceInstanceDecl)

---------------------------------------------
-- TYPECLASS CONSTRAINT PARSERS
---------------------------------------------

parseSurfaceClassApp :: Parser (SurfaceClassApp Span)
parseSurfaceClassApp =
   -- needs set notation first and backtrack since it can consume before discovering there is no ⋹
   try parseSurfaceClassAppSetNotation <|> parseSurfaceClassAppStandard

parseSurfaceClassContext :: Parser (SurfaceClassContext Span)
parseSurfaceClassContext =
    try parseSurfaceClassContextSetNotation -- needs backtrack since it can consume before discovering there is no ⋹
        <|> do
            open <- tok TokLParen
            apps <- concat <$> sepBy parseClassContextElement (tok TokComma)
            close <- tok TokRParen
            pure (SurfaceClassContext (combine open close) apps)
  where
    -- one element of a context: a set-notation context (a ⋹ Functor ⋒ Foldable),
    -- a single class app, or a parenthesized sub-context which flattens.
    parseClassContextElement :: Parser [SurfaceClassApp Span]
    parseClassContextElement =
        (sctxApps <$> try parseSurfaceClassContextSetNotation)
            <|> (one <$> parseSurfaceClassApp)
            <|> do
                tok_ TokLParen
                sub <- concat <$> sepBy parseClassContextElement (tok TokComma)
                tok_ TokRParen
                pure sub

-- a single class application, written as a standard application
parseSurfaceClassAppStandard :: Parser (SurfaceClassApp Span)
parseSurfaceClassAppStandard = do
    name <- parseIdentifier
    tys <- many parseSTAtom
    let fullSpan = case reverse tys of
          [] -> spanOf name
          (t:_) -> combine name t
    return $ SurfaceClassApp fullSpan name tys

-- a single class appplication, written as a constraint
-- ℤ ⋹ Additive
parseSurfaceClassAppSetNotation :: Parser (SurfaceClassApp Span)
parseSurfaceClassAppSetNotation = do
    tys <- some parseSTAtom
    elemOf <- tok TokTypeclassElement
    name <- parseIdentifier
    let fullSpan = combine3 (tys L.!! 0) elemOf name
    return $ SurfaceClassApp fullSpan name tys

-- a class context, written in set notation
-- a ⋹ Functor ⋒ Foldable
parseSurfaceClassContextSetNotation :: Parser (SurfaceClassContext Span)
parseSurfaceClassContextSetNotation = do
    tys <- some parseSTAtom
    elemOf <- tok TokTypeclassElement
    names <- sepBy1 parseIdentifier (tok TokTypeclassIntersect)
    let fullSpan = combine3 (tys L.!! 0) elemOf (L.last names)
    let apps = fmap (\n -> SurfaceClassApp fullSpan n tys) names
    return $ SurfaceClassContext fullSpan apps


---------------------------------------------
-- PRATT HELPERS
---------------------------------------------

-- builds the operator table for makeExprParser from a fixity environment
-- since makeExprParser forbids unary operators of equal precedence from coöcurring,
-- we need to merge all the unary operations into one parser which parses any number of them
-- and put that one last
mkOperatorTable ::
    (Located Identifier -> a -> a -> a) -> -- mkInfix: op lhs rhs -> node
    (Located Identifier -> a) -> -- mkIdent: op -> node
    (a -> a -> a) -> -- mkApp: f x -> node
    FixityEnv ->
    [[Operator Parser a]]
mkOperatorTable mkInfix mkIdent mkApp (FixityEnv rho) = binaryGroups <> unaryGroup
  where
    parseOneIdent :: Identifier -> Parser (Located Identifier)
    parseOneIdent ident = do
        t <- satisfy isIdent
        pure (Annotated (spanOf t) ident)
      where
        isIdent (Annotated _ (TokIdentifier ident')) = sameIdent ident ident'
        isIdent _ = False

    fixities = M.assocs rho
    (binaries, unaries) = L.partition (isBinary . snd) fixities

    -- a binary operator: (precedence, Operator).
    mkBinaryInfix (ident, fixity) = (prec, ctor (pfunc ident))
      where
        (ctor, prec) = case fixity of
            LeftAssocBinary n -> (InfixL, n)
            RightAssocBinary n -> (InfixR, n)
            NonAssocBinary n -> (InfixN, n)
            _ -> error "mkBinaryInfix: not a binary fixity (unreachable)"

    -- parse one operator ident and return the infix combining function.
    pfunc ident = do
        annIdent <- parseOneIdent ident
        pure (\lhs rhs -> mkInfix annIdent lhs rhs)

    -- group the binary operators by precedence, descending.
    binaryGroups =
        map (map snd)
            $ L.groupBy (\a b -> fst a == fst b)
            $ sortBy (comparing (Down . fst))
            $ map mkBinaryInfix binaries

    -- all unaries are PrefixUnary, so we discard the fixity and compress them
    -- into one Prefix operator that parses any of the unary idents.
    unaryGroup = case nonEmpty (map (parseOneIdent . fst) unaries) of
        Nothing -> []
        Just ne ->
            [ [ Prefix (unaryParser ne)] ]

    -- parse one-or-more prefix idents and fold them into an application chain.
    unaryParser ne = do
        idents <- some (foldl1' (<|>) ne)
        pure (\x -> foldl' mkApp x (map mkIdent idents))

-- are these two idents identical? we can't use the Eq instance here
-- since those consider raw and quoted the same, but in pratt quoted aren't legal ever
sameIdent :: Identifier -> Identifier -> Bool
sameIdent (IdentRaw a) (IdentRaw b) = a == b
sameIdent (IdentQuoted a) (IdentQuoted b) = a == b
sameIdent (IdentPrimitive a) (IdentPrimitive b) = a == b
sameIdent _ _ = False

mkTermOperatorTable :: FixityEnv -> [[Operator Parser (SurfaceExpr Span)]]
mkTermOperatorTable fixity =
    mkOperatorTable mkInfix mkIdent mkApp fixity
  where
    mkInfix :: Located Identifier -> SurfaceExpr Span -> SurfaceExpr Span -> SurfaceExpr Span
    mkInfix op lhs rhs = SEInfix (combine3 lhs op rhs) lhs op rhs
    mkIdent :: Located Identifier -> SurfaceExpr Span
    mkIdent op = SEIdentifier op
    mkApp :: SurfaceExpr Span -> SurfaceExpr Span -> SurfaceExpr Span
    mkApp f a = SEApp (combine f a) f a

mkTypeOperatorTable :: FixityEnv -> [[Operator Parser (SurfaceType Span)]]
mkTypeOperatorTable fixity =
    mkOperatorTable mkInfix mkIdent mkApp fixity
  where
    mkInfix :: Located Identifier -> SurfaceType Span -> SurfaceType Span -> SurfaceType Span
    mkInfix op lhs rhs = STApp (combine3 lhs op rhs) (STApp (combine lhs op) (STName op) lhs) rhs
    mkIdent :: Located Identifier -> SurfaceType Span
    mkIdent op = STName op
    mkApp :: SurfaceType Span -> SurfaceType Span -> SurfaceType Span
    mkApp f a = STApp (combine f a) f a

defaultTypeFixityEnv :: FixityEnv
defaultTypeFixityEnv = FixityEnv (one (IdentRaw "→", RightAssocBinary 5))

---------------------------------------------
-- ENTRY POINT
---------------------------------------------

parse :: [Located Token] -> Either ParseError (SurfaceModule Span, [ParseWarning])
parse toks = do
    let (termFix, typeFix) = collectFixities toks
    (m, _, warns) <- first bundleToParseError $
        runParser (runRWST parseModule (termFix, defaultTypeFixityEnv <> typeFix) ()) "" toks
    pure (m, warns)
    where

    bundleToParseError :: ParseErrorBundle [Located Token] Void -> ParseError
    bundleToParseError bundle =
      let pe = head (bundleErrors bundle)
      in ParseError (spanAtOffset (errorOffset pe)) ParseSyntaxError

    spanAtOffset :: Int -> Span
    spanAtOffset off =
      case dropWhile (\t -> posOffset (spanStart (spanOf t)) < fromIntegral off) toks of
        (t : _) -> spanOf t
        [] -> case reverse toks of
          (t : _) -> spanOf t
          [] -> error "spanAtOffset: empty token stream"
