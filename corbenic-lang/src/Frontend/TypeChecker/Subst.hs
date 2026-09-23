module Frontend.TypeChecker.Subst where

import Data.Map qualified as M
import Data.Set qualified as S
import Frontend.TypeChecker.Types

newtype Subst = Subst (Map TypeVar CorbenicType) deriving (Eq, Show)

instance One Subst where
    type OneItem Subst = (TypeVar, CorbenicType)
    one = Subst . one

instance Semigroup Subst where
    (Subst a) <> (Subst b) = Subst (M.map (apply (Subst a)) b `M.union` a)

instance Monoid Subst where
    mempty = Subst mempty

lookupSubst :: Subst -> TypeVar -> Maybe CorbenicType
lookupSubst (Subst s) tv = M.lookup tv s

lookupSubstDefault :: CorbenicType -> Subst -> TypeVar -> CorbenicType
lookupSubstDefault def s tv = maybe def id $ lookupSubst s tv

deleteSubst :: TypeVar -> Subst -> Subst
deleteSubst tv (Subst s) = Subst $ M.delete tv s

deleteManySubst :: [TypeVar] -> Subst -> Subst
deleteManySubst tvs s = foldr deleteSubst s tvs

extendSubst :: TypeVar -> CorbenicType -> Subst -> Subst
extendSubst tv ty = (one (tv, ty) <>)

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set TypeVar

instance (Substitutable a, Functor f, Foldable f) => Substitutable (f a) where
    apply s as = fmap (apply s) as
    ftv as = fold $ fmap ftv as

instance Substitutable CorbenicType where
    apply _ t@(CTVar (TypeVar (Rigid _) _ _)) = t
    apply s t@(CTVar tvar) = lookupSubstDefault t s tvar
    apply _ t@(CTCon _ _) = t
    apply _ t@(CTFam _ _) = t
    apply _ t@(CTPrim _ _) = t
    apply s (CTApp spn t1 t2) = CTApp spn (apply s t1) (apply s t2)
    apply s (CTTuple spn ts) = CTTuple spn $ apply s ts
    apply s (CTPred spn predicate) = CTPred spn $ apply s predicate
    apply s (CTForall spn tv ty) = CTForall spn tv $ apply (deleteSubst tv s) ty
    apply s (CTExists spn tv ty) = CTExists spn tv $ apply (deleteSubst tv s) ty
    apply s (CTConstrained spn preds ty) = CTConstrained spn (apply s preds) (apply s ty)

    ftv (CTVar tv) = one tv
    ftv (CTCon _ _) = mempty
    ftv (CTFam _ _) = mempty
    ftv (CTPrim _ _) = mempty
    ftv (CTApp _ t1 t2) = ftv t1 `S.union` ftv t2
    ftv (CTTuple _ cts) = ftv cts
    ftv (CTPred _ predicate) = ftv predicate
    ftv (CTForall _ tv ty) = S.delete tv $ ftv ty
    ftv (CTExists _ tv ty) = S.delete tv $ ftv ty
    ftv (CTConstrained _ preds ty) = ftv preds `S.union` ftv ty

instance Substitutable Pred where
    apply s (Pred spn ident tys) = Pred spn ident $ apply s tys
    apply s (Equal spn t1 t2) = Equal spn (apply s t1) (apply s t2)
    apply s (Quintessable spn t1 t2) = Quintessable spn (apply s t1) (apply s t2)

    ftv (Pred _ _ ts) = ftv ts
    ftv (Equal _ t1 t2) = ftv t1 `S.union` ftv t2
    ftv (Quintessable _ t1 t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
    apply s (Scheme tvs preds tys) = Scheme tvs (apply (deleteManySubst tvs s) preds) (apply (deleteManySubst tvs s) tys)
    ftv (Scheme tvs preds tys) = (ftv preds `S.union` ftv tys) `S.difference` (S.fromList tvs)
