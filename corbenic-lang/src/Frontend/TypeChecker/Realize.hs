module Frontend.TypeChecker.Realize where

import Control.Lens
import Control.Monad.Except
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Seed
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Frontend.TypeChecker.Subst
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface
import Data.Map qualified as M
import Data.Set qualified as S

-- temporary way to forbid rank n types before we implement them
-- probably imperfect, but whatever it doesn't have to be perfect
isHigherRank :: SurfaceType Span -> Bool
isHigherRank t = go (stripSpine t)
  where
    stripSpine (STForall _ _ ty) = stripSpine ty
    stripSpine (STExists _ _ ty) = stripSpine ty
    stripSpine (STConstrained _ _ ty) = stripSpine ty
    stripSpine ty = ty

    go (STName _) = False
    go (STApp _ a b) = go a || go b
    go (STFun _ a b) = go a || go b
    go (STList _ a) = go a
    go (STTuple _ as) = any go as
    go (STConstraint _ c) = go' c
    go (STConstrained {}) = True
    go (STForall {}) = True
    go (STExists {}) = True

    go' (SurfaceClassContext _ apps) = any go'' apps
    go'' (SurfaceClassApp _ _ ts) = any go ts

-- realize a parsed type into the internal type representation
realize :: SurfaceType Span -> Tc CorbenicType
realize st = case isHigherRank st of
    True -> throwError $ TypeCheckerError (spanOf st) (TCNYI "higher rank types")
    False -> case st of
        (STName (Annotated sp ident)) -> resolveTypeName sp ident
        (STApp sp t1 t2) -> do
            t1' <- realize t1
            t2' <- realize t2
            return $ CTApp sp t1' t2'
        (STFun sp t1 t2) -> do
            t1' <- realize t1
            t2' <- realize t2
            return $ mkFun sp t1' t2'
        (STList sp t) -> do
            t' <- realize t
            return $ mkList sp t'
        (STTuple sp tys) -> do
            tys' <- mapM realize tys
            return $ CTTuple sp tys'
        (STConstraint sp ctx) -> do
            preds <- realizeContext ctx
            pure (CTPred sp preds)
        (STConstrained sp ctx ty) -> do
            preds <- realizeContext ctx
            ty' <- realize ty
            pure (CTConstrained sp preds ty')
        (STForall sp idents ty) -> realizeQuantified CTForall sp (toList idents) (realize ty)
        (STExists sp idents ty) -> realizeQuantified CTExists sp (toList idents) (realize ty)

-- realize a parsed ∀ or ∃ (which can quantify over mutliple variables at once)
-- into a nested string of internal representation which can only quantify over one at a time
realizeQuantified :: (Span -> TypeVar -> CorbenicType -> CorbenicType) -> Span -> [Annotated Span Identifier] -> Tc CorbenicType -> Tc CorbenicType
realizeQuantified _ _ [] t = t
realizeQuantified ctor sp (b : bs) t = do
    (tv, t') <- bindToTVar b (realizeQuantified ctor sp bs t)
    return $ ctor sp tv t'

-- realize a class context into its predicates
realizeContext :: SurfaceClassContext Span -> Tc [Pred]
realizeContext (SurfaceClassContext _ apps) = mapM realizeClassApp apps

-- realize a single class application into its predicate
realizeClassApp :: SurfaceClassApp Span -> Tc Pred
realizeClassApp (SurfaceClassApp _ (Annotated sp name) args) = do
    cls <- M.lookup name <$> askFor tcClasses
    case cls of
        Nothing -> throwError $ TypeCheckerError sp (TCUnboundClass name)
        Just _ -> do
            args' <- mapM realize args
            pure (Pred sp name args')

-- resolve a type name. will check if, in order:
-- - this is locally scoped type variable
-- - this is a primitive type
-- - this is a type constructor
-- - this is a type class (and will be a constraint)
-- and will then throw an error if it couldn't find any
resolveTypeName :: Span -> Identifier -> Tc CorbenicType
resolveTypeName sp ident = do
    tv <- M.lookup ident <$> askFor tcTypeVars
    case tv of
        Just tv' -> return $ CTVar tv'
        Nothing -> do
            case M.lookup ident primitiveSeed of
                Just pt -> return $ CTPrim sp pt
                Nothing -> do
                    con <- M.lookup ident <$> askFor tcTyCons
                    case con of
                        Just _ -> return $ CTCon sp ident
                        Nothing -> do
                            cls <- M.lookup ident <$> askFor tcClasses
                            case cls of
                                Just _ -> return $ CTPred sp [Pred sp ident []]
                                Nothing -> throwError $ TypeCheckerError sp (TCUnboundTypeConstructor ident)



-- realize a type into a *reusable* scheme: leading quantifiers become fresh
-- metavariables. passes a continuation to run in which the surface
-- binder names are in scope
realizeScheme :: SurfaceType Span -> (Scheme -> Tc a) -> Tc (Scheme, a)
realizeScheme st k = do
    (scm, _, a) <- realizeSchemeBinders st k
    return (scm, a)

-- as realized scheme, but also returns a list of tuples of (quantified identifier, bound metavar)
realizeSchemeBinders :: SurfaceType Span -> (Scheme -> Tc a) -> Tc (Scheme, [(Located Identifier, TypeVar)], a)
realizeSchemeBinders = go [] [] []
    where
        go :: [TypeVar] -> [(Located Identifier, TypeVar)] -> [Pred] -> SurfaceType Span -> (Scheme -> Tc a) -> Tc (Scheme, [(Located Identifier, TypeVar)], a)
        go tvs binders preds (STForall _ ids ty) k =
            fmap snd (bindManyMVars (toList ids) $ \mvs ->
                go (tvs <> mvs) (binders <> zip (toList ids) mvs) preds ty k)
        go tvs binders preds (STConstrained _ ctx ty) k = do
            ps <- realizeContext ctx
            go tvs binders (preds <> ps) ty k
        go tvs binders preds ty k = do
            ty' <- realize ty
            let scm = Scheme tvs preds ty'
            a <- k scm
            return (scm, binders, a)

-- does the following:
-- - realize a signature into a reusable scheme (withRealizeSchemeBinders)
-- - skolemize it, replacing all quantified metavariables with fresh skolems
-- - runs the continuation against the *skolemized* scheme's body type and predicates, with all
--   identifiers bound to the skolems
-- - returns the *original* reusable scheme and the fresh skolems, so the caller can decide
--   whether to emit the predicates and check for escape
realizeSkolemizedScheme :: SurfaceType Span -> (CorbenicType -> [Pred] -> Tc a) -> Tc (Scheme, [TypeVar], a)
realizeSkolemizedScheme st k = do
    (scm@(Scheme mvs preds bodyTy), binders, _) <- realizeSchemeBinders st return
    -- make a new rigid for each quantified metavariable
    rigids <- traverse (\mv -> freshRigidTV (spanOf mv) (tvKind mv)) mvs
    -- prepare a substitution that binds each mvar to its rigid
    let skolemSubst = Subst (M.fromList (zip mvs (fmap CTVar rigids)))
        preds' = apply skolemSubst preds
        bodyTy' = apply skolemSubst bodyTy
    -- rebind the surface names to the skolems for the duration of the check
    let rebind = over tcTypeVars $ \env ->
            foldr (\(Annotated _ ident, r) -> M.insert ident r) env (zip (fmap fst binders) rigids)
    -- perform the body check
    a <- local rebind (k bodyTy' preds')
    return (scm, rigids, a)

-- reconstruct the (possibly quantified and/or constrained) type denoted by a scheme
schemeToType :: Scheme -> CorbenicType
schemeToType (Scheme tvs preds body) =
    let body' = case preds of
            [] -> body
            _ -> CTConstrained (spanOf body) preds body
    in foldr (\tv t -> CTForall (spanOf tv) tv t) body' tvs

-- check if any of the given skolems breached containment and
-- entered the term environment when they are not supposed to
checkSkolemEscape :: Span -> Set TypeVar -> Tc ()
checkSkolemEscape sp skolems = do
    s <- use currentSubst
    env <- askFor tcTerms
    let leaked = ftv (apply s env) `S.intersection` skolems
    unless (S.null leaked) $
        throwError $ TypeCheckerError sp (TCSkolemEscape (CTVar (S.findMin leaked)))
