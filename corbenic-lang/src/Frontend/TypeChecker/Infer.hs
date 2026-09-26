module Frontend.TypeChecker.Infer where

import Control.Lens
import Control.Monad.Except
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Subst
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Frontend.TypeChecker.Realize
import Frontend.TypeChecker.Unify
import Syntax.Location
import Syntax.Surface
import Syntax.Literal
import Syntax.Identifier
import Data.Set qualified as S
import Data.Map qualified as M
import Data.List.NonEmpty qualified as NE


-- create fresh names for each variable in the scheme, turn it into a type
instantiate :: Scheme -> Tc CorbenicType
instantiate (Scheme vs preds ty) = do
    -- generate new tvars with the same span and kind for instantiation
    vs' <- traverse (\(TypeVar _ spn k) -> freshMetavar spn k) vs
    let s = Subst $ fromList $ zip vs vs'
    -- assert the instantiated constraints
    tellPreds $ apply s preds
    -- instantiate the type
    return $ apply s ty


-- extract all universally quantified vars and predicates
-- from a type
unrollSpine :: CorbenicType -> ([TypeVar], [Pred], CorbenicType)
unrollSpine (CTForall _ tv body) = let
    (tvs, preds, body') = unrollSpine body
    in (tv : tvs, preds, body')
unrollSpine (CTConstrained _ ps body) = let
    (tvs, preds, body') = unrollSpine body
    in (tvs, ps <> preds, body')
unrollSpine ty = ([], [], ty)

-- close over all free non-rigid variables in the type, generating a scheme.
-- if the type begins with a sequence of ∀ or constraints, lift those into the
-- scheme as well
generalize :: CorbenicType -> Tc Scheme
generalize t = do
    let (tvs, preds, body) = unrollSpine t
    rho <- askFor tcTerms
    s <- use currentSubst
    let body' = apply s body
    -- don't re-quantify the spine's own binders: they are already in tvs
    let bound = S.fromList tvs
    let free = ftv body' `S.difference` ftv (apply s rho) `S.difference` bound
    let free' = S.filter isMetavar free
    return $ Scheme (tvs <> toList free') (apply s preds) body'

-- convert a type into a monomorphic scheme
-- as generalize, but don't quantify free metavars
-- right now this is unused, but in the distant future (GADTs) we will need this
monomorphize :: CorbenicType -> Scheme
monomorphize t = let (tvs, preds, body) = unrollSpine t in Scheme tvs preds body

-- attempt to infer a type for an expression
infer :: SurfaceExpr Span -> Tc (SurfaceExpr (Span, CorbenicType))
infer (SELiteral l) = inferLiteral l
infer (SEIdentifier (Annotated sp ident)) = do
    rho <- askFor tcTerms
    case M.lookup ident rho of
        Just scm -> do
            t <- instantiate scm
            return $ SEIdentifier (Annotated (sp, t) ident)
        Nothing -> throwError $ TypeCheckerError sp (TCUnboundIdentifier ident)
infer (SELambda sp (Annotated spi ident) body) = do
    a <- freshMetavarTV spi CKStar
    let a' = CTVar a
    b <- freshMetavar (spanOf body) CKStar
    let withTerm = over tcTerms (M.insert ident (Scheme [] [] a'))
    body' <- local withTerm $ check body b
    let ident' = Annotated (spi, a') ident
    let t = mkFun sp a' b
    return $ SELambda (sp, t) ident' body'
infer (SEApp sp f x) = do
    f' <- infer f
    x' <- infer x
    let tf = exprType f'
    let tx = exprType x'
    r <- freshMetavar sp CKStar
    unify tf (mkFun sp tx r)
    return $ SEApp (sp, r) f' x'
infer (SETypeLambda sp (Annotated spi ident) body) = do
    tv <- freshKind >>= freshMetavarTV spi
    let withTypeVar = over tcTypeVars (M.insert ident tv)
    body' <- local withTypeVar (infer body)
    let ident' = Annotated (spi, CTVar tv) ident
    let t = CTForall sp tv (exprType body')
    return $ SETypeLambda (sp, t) ident' body'
infer (SETypeApp sp e t) = do
    e' <- infer e
    t' <- realize t
    case exprType e' of
        CTForall _ v body -> do
            -- local substitution, not a global unify so
            -- the same polymorphic value can be applied
            -- at many types
            -- TODO: verify the kinds of v and t' match
            let result = apply (one (v, t')) body
            return $ SETypeApp (sp, result) e' t
        _ -> throwError $ TypeCheckerError sp (TCIllegalTypeApp (exprType e'))
infer (SEWhere sp e decls) = do
    generalizeWhereDecls decls $ \decls' -> do
        e' <- infer e
        return $ SEWhere (sp, exprType e') e' decls'
infer (SEDo sp sdis) = checkDo sp sdis Nothing
infer (SECase sp scrut branches) = do
    scrut' <- infer scrut
    let scrutTy = exprType scrut'
    r <- freshMetavar sp CKStar
    branches' <- mapM (checkBranch scrutTy r) branches
    return $ SECase (sp, r) scrut' branches'
infer (SELambdaCase sp branches) = do
    scrutTy <- freshMetavar sp CKStar
    r <- freshMetavar sp CKStar
    branches' <- mapM (checkBranch scrutTy r) branches
    return $ SELambdaCase (sp, mkFun sp scrutTy r) branches'
infer (SEList sp es) = do
    a <- freshMetavar sp CKStar
    es' <- mapM (`check` a) es
    return $ SEList (sp, mkList sp a) es'
infer (SETuple sp es) = do
    es' <- mapM infer es
    let t = CTTuple sp $ fmap exprType es'
    return $ SETuple (sp, t) es'
infer (SEOpSectionL sp e (Annotated spi ident)) = do
    e' <- infer e
    opSchm <- M.lookup ident <$> askFor tcTerms
    case opSchm of
        Nothing -> throwError $ TypeCheckerError spi $ TCUnboundIdentifier ident
        Just schm -> do
            opTy <- instantiate schm
            let a = exprType e'
            b <- freshMetavar sp CKStar
            c <- freshMetavar sp CKStar
            unify opTy (mkFun sp a (mkFun sp b c))
            return $ SEOpSectionL (sp, mkFun sp b c) e' (Annotated (spi, opTy) ident)
infer (SEOpSectionR sp (Annotated spi ident) e) = do
    e' <- infer e
    opSchm <- M.lookup ident <$> askFor tcTerms
    case opSchm of
        Nothing -> throwError $ TypeCheckerError spi $ TCUnboundIdentifier ident
        Just schm -> do
            opTy <- instantiate schm
            a <- freshMetavar sp CKStar
            let b = exprType e'
            c <- freshMetavar sp CKStar
            unify opTy (mkFun sp a (mkFun sp b c))
            return $ SEOpSectionR (sp, mkFun sp a c) (Annotated (spi, opTy) ident) e'
infer (SEInfix sp l operand r) = infer (SEApp sp (SEApp sp (SEIdentifier operand) l) r)
infer (SEAnnotation ann e sty) = do
    (scm, skolems, e') <- realizeSkolemizedScheme sty $ \bodyTy _ -> check e bodyTy
    checkSkolemEscape ann (S.fromList skolems)
    return $ SEAnnotation (ann, schemeToType scm) e' sty
infer (SEHole sp) = freshMetavar sp CKStar >>= \t -> return . SEHole $ (sp, t)


-- check an expression against a known type
check :: SurfaceExpr Span -> CorbenicType -> Tc (SurfaceExpr (Span, CorbenicType))
check e@(SELiteral _) t = dumbCheck e t
check e@(SEIdentifier _) t = dumbCheck e t
check (SELambda sp (Annotated spi ident) body) t = do
    a <- freshMetavarTV spi CKStar
    let a' = CTVar a
    b <- freshMetavar (spanOf body) CKStar
    unify t (mkFun sp a' b)
    let withTerm = over tcTerms (M.insert ident (Scheme [] [] a'))
    body' <- local withTerm (check body b)
    let ident' = Annotated (spi, a') ident
    return $ SELambda (sp, mkFun sp a' b) ident' body'
check (SEApp sp f x) t = do
    a <- freshMetavar (spanOf x) CKStar
    let t' = mkFun sp a t
    f' <- check f t'
    x' <- check x a
    return $ SEApp (sp, t) f' x'
check (SETypeLambda sp i@(Annotated spi ident) body) t = do
    case t of
        CTForall _ tv bt
            | isRigid tv -> do
                -- the expected binder is already a skolem: bind ident to it directly
                body' <- local (over tcTypeVars (M.insert ident tv)) (check body bt)
                let ty = CTForall sp tv (exprType body')
                return $ SETypeLambda (sp, ty) (Annotated (spi, CTVar tv) ident) body'
            | otherwise -> do
                -- the expected binder is a metavar: bind ident to a fresh rigid,
                (rv, body') <- bindToRigid i (check body bt)
                -- then bind the metavar to it
                -- TODO: verify that tv and rv have the same kind
                unify (CTVar tv) (CTVar rv)
                let ty = CTForall sp rv (exprType body')
                return $ SETypeLambda (sp, ty) (Annotated (spi, CTVar rv) ident) body'
        _ -> do
            -- this is a typeerror, this should always be a forall.
            -- for maximal error specificity we check against a fresh type
            -- so the error message is still good. redundant work, but only in error case so its fine
            b <- freshMetavar sp CKStar
            (rv, body') <- bindToRigid i (check body b)
            let ty = CTForall sp rv (exprType body')
            throwError $ TypeCheckerError sp (TCCouldNotUnify ty t)
check e@(SETypeApp {}) t = dumbCheck e t
check (SEWhere sp e decls) t = do
    generalizeWhereDecls decls $ \decls' -> do
        e' <- check e t
        return $ SEWhere (sp, t) e' decls'
check (SEDo sp sdis) t = checkDo sp sdis (Just t)
check (SECase sp scrut branches) t = do
  scrut' <- infer scrut
  let scrutTy = exprType scrut'
  branches' <- mapM (checkBranch scrutTy t) branches
  return $ SECase (sp, t) scrut' branches'
check (SELambdaCase sp branches) t = do
    scrutTy <- freshMetavar sp CKStar
    r <- freshMetavar sp CKStar
    let t' = mkFun sp scrutTy r
    unify t t'
    branches' <- mapM (checkBranch scrutTy r) branches
    return $ SELambdaCase (sp, t') branches'
check (SEList sp es) t = do
    a <- freshMetavar sp CKStar
    unify t (mkList sp a)
    es' <- mapM (`check` a) es
    return $ SEList (sp, mkList sp a) es'
check (SETuple sp es) t = do
    ts <- mapM (\e -> freshMetavar (spanOf e) CKStar) es
    unify t (CTTuple sp ts)
    es' <- zipWithMNE check es ts
    return $ SETuple (sp, CTTuple sp ts) es'
check e@(SEOpSectionL {}) t = dumbCheck e t
check e@(SEOpSectionR {}) t = dumbCheck e t
check (SEInfix sp l operand r) t = check (SEApp sp (SEApp sp (SEIdentifier operand) l) r) t
check (SEAnnotation ann e sty) t = do
    (scm, skolems, e') <- realizeSkolemizedScheme sty $ \bodyTy _ -> check e bodyTy
    checkSkolemEscape ann (S.fromList skolems)
    let ty = schemeToType scm
    unify t ty
    return $ SEAnnotation (ann, ty) e' sty
check (SEHole sp) t = return $ SEHole (sp, t)

-- check a term declaration with an optional signature
checkTermDecl :: Maybe (SurfaceTypeDecl Span) -> SurfaceTermDecl Span -> Tc (Scheme, SurfaceTermDecl (Span, CorbenicType))
checkTermDecl mSig d@(SurfaceTermDecl sp _ (Annotated spi ident) body) = do
    (scm, body') <- case mSig of
        Nothing -> do
           body' <- infer body
           scm <- generalize $ exprType body'
           return (scm, body')
        Just sig -> checkAgainstSignature (stydType sig) body
    return (scm, d { stdBody = body', stdAnn = (sp, exprType body'), stdName = Annotated (spi, exprType body') ident})


checkAgainstSignature :: SurfaceType Span -> SurfaceExpr Span -> Tc (Scheme, SurfaceExpr (Span, CorbenicType))
checkAgainstSignature sig body = do
    -- realize the signature into a reusable scheme, then check the body against a
    -- skolemized view of it (see realizeSkolemizedScheme)
    (scm, skolems, body') <- realizeSkolemizedScheme sig $ \bodyTy preds -> do
        tellPreds preds
        check body bodyTy
    checkSkolemEscape (spanOf sig) (S.fromList skolems)
    return (scm, body')


-- Check a where block's declarations and run a continuation with the schemes in scope and the typed decls
generalizeWhereDecls :: [SurfaceWhereDeclaration Span]
                     -> ([SurfaceWhereDeclaration (Span, CorbenicType)] -> Tc a)
                     -> Tc a
generalizeWhereDecls decls k = do
      let binds = [ (name, stydType <$> mSig, body)
                  | SWDTerm mSig (SurfaceTermDecl _ _ (Annotated _ name) body) <- decls ]
      checkKnot binds $ \finalized -> do
          let rewrapped =
                  [ SWDTerm mSig (SurfaceTermDecl (sp, bt) doc (Annotated (spi, bt) name) body')
                  | (SWDTerm mSig (SurfaceTermDecl sp doc (Annotated spi name) _), (_, _, body')) <- zip decls finalized
                  , let bt = exprType body' ]
          k rewrapped


-- check a branch against the given scrutinee's type and result type
-- all binds generated in patterns are monomorphic
checkBranch :: CorbenicType -> CorbenicType -> SurfaceBranch Span -> Tc (SurfaceBranch (Span, CorbenicType))
checkBranch scrutTy resultTy (SurfaceBranch sp pat body) = do
    (pat', binds) <- checkPattern pat scrutTy
    let schemes = fmap (Scheme [] []) binds
    body' <- local (over tcTerms (M.union schemes)) (check body resultTy)
    return $ SurfaceBranch (sp, resultTy) pat' body'

-- check a pattern against the type of the value it matches
-- returns both the typed pattern and a map of its bindings' types
checkPattern :: SurfacePattern Span -> CorbenicType
             -> Tc (SurfacePattern (Span, CorbenicType), Map Identifier CorbenicType)
checkPattern (SPLiteral (Annotated sp l)) t = do
    ty <- exprType <$> inferLiteral (Annotated sp l)
    unify t ty
    return (SPLiteral (Annotated (sp, t) l), mempty)
checkPattern (SPVar (Annotated sp ident)) t = return (SPVar (Annotated (sp, t) ident), one (ident, t))
checkPattern (SPWild sp) t = return (SPWild (sp, t), mempty)
checkPattern (SPCon sp (Annotated spi ident) ps) t = do
    scms <- askFor tcTerms
    case M.lookup ident scms of
        Nothing -> throwError $ TypeCheckerError spi $ TCUnboundTypeConstructor ident
        Just scm -> do
            ctorTy <- instantiate scm
            let (args, resultTy) = peelFun ctorTy
            let expArity = genericLength args
            let gotArity = genericLength ps
            case expArity == gotArity of
                False -> throwError $ TypeCheckerError spi $ TCPatternArity ident expArity gotArity
                True -> do
                    unify t resultTy
                    ps' <- zipWithM checkPattern ps args
                    let (pats, bindss) = unzip ps'
                    return (SPCon (sp, t) (Annotated (spi, t) ident) pats, fold bindss)
checkPattern (SPList sp ps) t = do
    a <- freshMetavar sp CKStar
    unify t $ mkList sp a
    ps' <- mapM (`checkPattern` a) ps
    let (pats, bindss) = unzip ps'
    return (SPList (sp, t) pats, fold bindss)
checkPattern (SPTuple sp ps) t = do
    vars <- replicateM (length ps) (freshMetavar sp CKStar)
    case nonEmpty vars of
        Nothing -> throwError $ TypeCheckerError sp $ TCBug "Empty tuple type"
        Just vars' -> do
            unify t (CTTuple (spanOf t) vars')
            ps' <- zipWithMNE checkPattern ps vars'
            return (SPTuple (sp, t) (fmap fst ps'), foldMap snd ps')


-- check a do block against a (possibly inferred) result type
checkDo :: Span -> NonEmpty (SurfaceDoInstruction Span) -> Maybe CorbenicType -> Tc (SurfaceExpr (Span, CorbenicType))
checkDo sp sdis mt = do
  m <- freshMetavar sp (CKArr CKStar CKStar)
  -- check if Monad is in scope if it isn't, throw an error
  -- we don't use realizeClassApp here since that needs
  -- a SurfaceType which we don't have
  cs <- askFor tcClasses
  case M.lookup (IdentRaw "Monad") cs of
    Nothing -> throwError $ TypeCheckerError sp $ TCUnboundClass (IdentRaw "Monad")
    Just _ -> tellPreds [Pred (spanOf m) (IdentRaw "Monad") [m]]
  a <- freshMetavar sp CKStar
  let ma = CTApp sp m a
  case mt of
    Nothing -> pass
    Just t -> unify t ma
  sdis' <- checkDoBlocks m a sdis
  return $ SEDo (sp, ma) sdis'


-- check a list of do instructions, in order, with a given monad and last-line result type
checkDoBlocks :: CorbenicType -> CorbenicType -> NonEmpty (SurfaceDoInstruction Span) -> Tc (NonEmpty (SurfaceDoInstruction (Span, CorbenicType)))
checkDoBlocks monad t sdiNE = case NE.uncons sdiNE of
  (SDIMonadicStmt e, Nothing) -> one . SDIMonadicStmt <$> check e (CTApp (spanOf e) monad t)
  (e, Nothing) -> throwError $ TypeCheckerError (spanOf e) TCBadDoBlockEnding
  (SDIMonadicStmt e, Just sdis) -> do
    -- todo: warn if a isn't unit, somehow
    a <- freshMetavar (spanOf e) CKStar
    e' <- check e (CTApp (spanOf e) monad a)
    sdis' <- checkDoBlocks monad t sdis
    return $ SDIMonadicStmt e' `NE.cons` sdis'
  (SDIBindName sp (Annotated spi ident) body, Just sdis) -> do
    body' <- infer body
    let bodyTy = exprType body'
    let bodyScheme = Scheme [] [] bodyTy
    sdis' <- local (over tcTerms (M.insert ident bodyScheme)) (checkDoBlocks monad t sdis)
    let this = SDIBindName (sp, bodyTy) (Annotated (spi, bodyTy) ident) body'
    return $ this `NE.cons` sdis'
  (SDIExtractMonad sp (Annotated spi ident) body, Just sdis) -> do
    a <- freshMetavar spi CKStar
    let ma = CTApp (spanOf body) monad a
    body' <- check body ma
    let aSchm = Scheme [] [] a
    sdis' <- local (over tcTerms (M.insert ident aSchm)) (checkDoBlocks monad t sdis)
    let this = SDIExtractMonad (sp, ma) (Annotated (spi, a) ident) body'
    return $ this `NE.cons` sdis'


-- helpers begin here

-- convert a curried function into a list of its arguments
peelFun :: CorbenicType -> ([CorbenicType], CorbenicType)
peelFun ts = (reverse $ tail ts', head ts') where
    ts' = go ts
    go (CTApp _ (CTApp _ (CTPrim _ PFunction) a) b) = go b <> one a
    go t = one t

-- monadic zipWith over nonempty, because this isn't in the prelude???
zipWithMNE :: Monad m => (a -> b -> m c) -> NonEmpty a -> NonEmpty b -> m (NonEmpty c)
zipWithMNE f (a :| as) (b :| bs) = do
    hd <- f a b
    tl <- zipWithM f as bs
    return $ hd :| tl


-- the type of a typed expression node, fetched from its annotation
exprType :: SurfaceExpr (Span, CorbenicType) -> CorbenicType
exprType = snd . (^. exprAnn)

-- do a dumb check: infer and then unify with inferred type
dumbCheck :: SurfaceExpr Span -> CorbenicType -> Tc (SurfaceExpr (Span, CorbenicType))
dumbCheck e t = do
    e' <- infer e
    unify (exprType e') t
    return e'

-- generate the type for Quintessable literals (which is to say, all of them)
mkQuintessableLiteral :: Located Literal -> PrimType -> Tc (SurfaceExpr (Span, CorbenicType))
mkQuintessableLiteral (Annotated sp l) p = do
    a <- freshMetavar sp CKStar
    let t = CTConstrained sp [Quintessable sp (CTPrim sp p) a] a
    return $ SELiteral (Annotated (sp, t) l)

inferLiteral :: Located Literal -> Tc (SurfaceExpr (Span, CorbenicType))
inferLiteral l@(Annotated _ (LitNatural _)) = mkQuintessableLiteral l PNatural
inferLiteral (Annotated sp (LitRational l)) = do
    -- not a bare primitive (it's Ratio ℤ), so do this one explicitly
    a <- freshMetavar sp CKStar
    let q = CTApp sp (CTPrim sp PRatio) (CTPrim sp PInteger)
    let t = CTConstrained sp [Quintessable sp q a] a
    return $ SELiteral (Annotated (sp, t) (LitRational l))
inferLiteral l@(Annotated _ (LitBool _)) = mkQuintessableLiteral l PBool
inferLiteral l@(Annotated _ (LitChar _)) = mkQuintessableLiteral l PChar
inferLiteral l@(Annotated _ (LitText _)) = mkQuintessableLiteral l PText

-- helper for checking mutually recursive bindings
-- used in for example class/instance declarations, where clauses, etc
checkKnot :: [(Identifier, Maybe (SurfaceType Span), SurfaceExpr Span)] -- ident, maybe annotation
          -> ([(Identifier, Scheme, SurfaceExpr (Span, CorbenicType))] -> Tc a) -- continuation
          -> Tc a
checkKnot binds k = do
    -- seed by either realizing the signature's scheme or ginning up a fresh monomorphic scheme
    seeded <- forM binds $ \(name, mSig, body) -> do
        scm <- case mSig of
            Just sig -> fst <$> realizeScheme sig return
            Nothing -> do
                v <- freshMetavarTV (spanOf body) CKStar
                return $ Scheme [v] [] (CTVar v)
        return (name, scm)

    -- check bodies in the seeded environment
    checked <- local (over tcTerms (M.union (M.fromList seeded))) $ forM binds $ \(_, mSig, body) -> do
        case mSig of
            Just sig -> first Just <$> checkAgainstSignature sig body
            Nothing -> (Nothing,) <$> infer body

    -- unify the fresh metavars against the body types and generalize
    finalized <- forM (zip seeded checked) $ \((name, seedScm), (mScm, body')) -> do
        scm <- case (seedScm, mScm) of
            (Scheme [_] _ v, Nothing) -> do
                let bt = exprType body'
                unify v bt
                generalize bt
            (_, Just scm) -> return scm
            _ -> throwError $ TypeCheckerError (spanOf body') $ TCBug "checkKnot got an invalid scheme at finalize"
        return (name, scm, body')

    -- re-seed with the generalized schemes and run the continuation
    let finalBinds = [(name, scm) | (name, scm, _) <- finalized]
    local (over tcTerms (M.union (M.fromList finalBinds))) (k finalized)
