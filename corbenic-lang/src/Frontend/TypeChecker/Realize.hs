module Frontend.TypeChecker.Realize where

import Control.Monad.Except
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Seed
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface
import Data.Map qualified as M

-- temporary way to forbid rank n types before we implement them
-- probably imperfect, but whatever it doesn't have to be perfect
isHigherRank :: SurfaceType ann -> Bool
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
    go (STConstrained _ _ _) = True
    go (STForall _ _ _) = True
    go (STExists _ _ _) = True

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



-- realize a type into a scheme (pulling out the relevant quantifiers/skolems)
-- passes a continuation to run in which everything is bound
realizeScheme :: SurfaceType Span -> (Scheme -> Tc a) -> Tc (Scheme, a)
realizeScheme = go [] []
    where
        go :: [TypeVar] -> [Pred] -> SurfaceType Span -> (Scheme -> Tc a) -> Tc (Scheme, a)
        go tvs preds (STForall _ ids ty) k = fmap snd (bindManyRigids (toList ids) $ \skolems -> go (tvs <> skolems) preds ty k)
        go tvs preds (STConstrained _ ctx ty) k = do
            ps <- realizeContext ctx
            go tvs (preds <> ps) ty k
        go tvs preds ty k = do
            ty' <- realize ty
            let scm = Scheme tvs preds ty'
            k scm >>= return . (scm,)
