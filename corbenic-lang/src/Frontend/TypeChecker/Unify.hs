module Frontend.TypeChecker.Unify (unify) where

import Frontend.TypeChecker.Subst
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Syntax.Location
import Data.Set qualified as S
import Frontend.TypeChecker.Error
import Control.Monad.Except
import Control.Lens
import Relude.Unsafe (fromJust)

bind :: TypeVar -> CorbenicType -> Tc ()
bind n t
    | isRigid n = throwError $ TypeCheckerError (spanOf n) (TCBug "bind on a rigid")
    | t == CTVar n = pure ()
    | n `S.member` ftv t = throwError (TypeCheckerError (spanOf n) (TCInfiniteType n t))
    | otherwise = currentSubst %= extendSubst n t

-- wrapper around the unifier, which applies the current subst
unify :: CorbenicType -> CorbenicType -> Tc ()
unify t1 t2 = do
    cs <- use currentSubst
    let t1' = apply cs t1
    let t2' = apply cs t2
    unify' t1' t2'

-- the actual unifier
unify' :: CorbenicType -> CorbenicType -> Tc ()
unify' (CTVar a) t = unifyVar a t
unify' t (CTVar a) = unifyVar a t
unify' (CTCon _ a) (CTCon _ b) | a == b = pass
unify' (CTFam _ a) (CTFam _ b) | a == b = pass
unify' (CTPrim _ a) (CTPrim _ b) | a == b = pass
unify' (CTApp _ f x) (CTApp _ f' x') = unify f f' *> unify x x'
unify' (CTTuple _ as) (CTTuple _ as') | length as == length as' = zipWithM_ unify as as'
unify' (CTPred _ preds) (CTPred _ _) = throwError $ rankNError $ fromJust $ viaNonEmpty head preds
unify' (CTForall _ q1 _) (CTForall _ _ _) = throwError $ rankNError q1
unify' (CTExists _ q1 _) (CTExists _ _ _) = throwError $ rankNError q1
unify' (CTConstrained _ preds _) (CTConstrained _ _ _) = throwError $ rankNError $ fromJust $ viaNonEmpty head preds
unify' a b = throwError (TypeCheckerError (spanOf a) (TCCouldNotUnify a b))

-- unify a TypeVar with anything
unifyVar :: TypeVar -> CorbenicType -> Tc ()
unifyVar a (CTVar b) = unifyVars' a b
unifyVar a t
    | isMetavar a = bind a t
    | otherwise = throwError (TypeCheckerError (spanOf a) (TCCouldNotUnify (CTVar a) t))


-- unify a TypeVar with another TypeVar
-- handles the proper rigid checking
unifyVars' :: TypeVar -> TypeVar -> Tc ()
unifyVars' n@(TypeVar (Metavar _) _ _) t = bind n (CTVar t)
unifyVars' t n@(TypeVar (Metavar _) _ _) = bind n (CTVar t)
unifyVars' (TypeVar (Rigid a) _ _) (TypeVar (Rigid b) _ _) | a == b = pass
unifyVars' a b = throwError $ TypeCheckerError (spanOf a) $ TCCouldNotUnify (CTVar a) (CTVar b)

-- safety: preds call this with an unsafe fromJust, but the given is always nonempty (known at parse time),
-- being lazy since this is a temporary error
-- and will be removed once rank n exists
rankNError :: HasSpan a => a -> TypeCheckerError
rankNError a = TypeCheckerError (spanOf a) (TCNYI "rank-n types")
