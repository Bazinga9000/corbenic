module Frontend.TypeChecker.KindSubst where

import Data.Map qualified as M
import Data.Set qualified as S
import Frontend.TypeChecker.Types

newtype KSubst = KSubst (Map KindVar CorbenicKind) deriving (Eq, Show)

instance One KSubst where
    type OneItem KSubst = (KindVar, CorbenicKind)
    one = KSubst . one

instance Semigroup KSubst where
    (KSubst a) <> (KSubst b) = KSubst (M.map (applyK (KSubst a)) b `M.union` a)

instance Monoid KSubst where
    mempty = KSubst mempty

lookupKSubst :: KSubst -> KindVar -> Maybe CorbenicKind
lookupKSubst (KSubst s) tv = M.lookup tv s

lookupKSubstDefault :: CorbenicKind -> KSubst -> KindVar -> CorbenicKind
lookupKSubstDefault def s tv = fromMaybe def $ lookupKSubst s tv

deleteKSubst :: KindVar -> KSubst -> KSubst
deleteKSubst tv (KSubst s) = KSubst $ M.delete tv s

deleteManyKSubst :: [KindVar] -> KSubst -> KSubst
deleteManyKSubst tvs s = foldr deleteKSubst s tvs

extendKSubst :: KindVar -> CorbenicKind -> KSubst -> KSubst
extendKSubst tv ty = (one (tv, ty) <>)

class KSubstitutable a where
    applyK :: KSubst -> a -> a
    fkv :: a -> Set KindVar

instance (KSubstitutable a, Functor f, Foldable f) => KSubstitutable (f a) where
    applyK s = fmap (applyK s)
    fkv = foldMap fkv

instance KSubstitutable PrimType where
    applyK = const id
    fkv = const mempty

instance KSubstitutable CorbenicKind where
    applyK s (CKVar kv) = maybe (CKVar kv) (applyK s) (lookupKSubst s kv)
    applyK s (CKArr k1 k2) = CKArr (applyK s k1) (applyK s k2)
    applyK _ k = k

    fkv (CKVar kv) = one kv
    fkv (CKArr k1 k2) = fkv k1 `S.union` fkv k2
    fkv _ = mempty

instance KSubstitutable TypeVar where
    applyK s (TypeVar name sp k) = TypeVar name sp $ applyK s k
    fkv (TypeVar _ _ k) = fkv k

instance KSubstitutable CorbenicType where
    applyK s (CTVar tv) = CTVar $ applyK s tv
    applyK _ t@(CTCon _ _) = t
    applyK _ t@(CTFam _ _) = t
    applyK _ t@(CTPrim _ _) = t
    applyK s (CTApp spn t1 t2) = CTApp spn (applyK s t1) (applyK s t2)
    applyK s (CTTuple spn ts) = CTTuple spn $ applyK s ts
    applyK s (CTPred spn predicate) = CTPred spn $ applyK s predicate
    applyK s (CTForall spn tv ty) = CTForall spn (applyK s tv) (applyK s ty)
    applyK s (CTExists spn tv ty) = CTExists spn (applyK s tv) (applyK s ty)
    applyK s (CTConstrained spn preds ty) = CTConstrained spn (applyK s preds) (applyK s ty)


    fkv (CTVar tv) = fkv tv
    fkv (CTCon _ _) = mempty
    fkv (CTFam _ _) = mempty
    fkv (CTPrim _ _) = mempty
    fkv (CTApp _ t1 t2) = fkv t1 `S.union` fkv t2
    fkv (CTTuple _ cts) = fkv cts
    fkv (CTPred _ predicate) = fkv predicate
    fkv (CTForall _ tv ty) = fkv ty `S.union` fkv tv
    fkv (CTExists _ tv ty) = fkv ty `S.union` fkv tv
    fkv (CTConstrained _ preds ty) = fkv preds `S.union` fkv ty

instance KSubstitutable Pred where
    applyK s (Pred spn ident tys) = Pred spn ident $ applyK s tys
    applyK s (Equal spn t1 t2) = Equal spn (applyK s t1) (applyK s t2)
    applyK s (Quintessable spn t1 t2) = Quintessable spn (applyK s t1) (applyK s t2)

    fkv (Pred _ _ ts) = fkv ts
    fkv (Equal _ t1 t2) = fkv t1 `S.union` fkv t2
    fkv (Quintessable _ t1 t2) = fkv t1 `S.union` fkv t2

instance KSubstitutable Scheme where
    applyK s (Scheme tvs preds tys) = Scheme tvs (applyK s preds) (applyK s tys)
    fkv (Scheme tvs preds tys) = (fkv preds `S.union` fkv tys) `S.difference` S.fromList kvs where
        kvs = catMaybes $ do
            tv <- tvs
            case tv of
                (TypeVar _ _ (CKVar kv)) -> return $ Just kv
                _ -> return Nothing
