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
unify' (CTVar n) t = bind n t
unify' t (CTVar n) = bind n t
unify' (CTRigid a) (CTRigid b) | a == b = pass
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


-- safety: preds call this with an unsafe fromJust, but the given is always nonempty (known at parse time),
-- being lazy since this is a temporary error
-- and will be removed once rank n exists
rankNError :: HasSpan a => a -> TypeCheckerError
rankNError a = TypeCheckerError (spanOf a) (TCNYI "rank-n types")
