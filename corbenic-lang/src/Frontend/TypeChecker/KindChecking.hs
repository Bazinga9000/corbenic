module Frontend.TypeChecker.KindChecking where

import Control.Lens
import Control.Monad.Except
import Data.Map qualified as M
import Data.Set qualified as S
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Frontend.TypeChecker.KindSubst
import Syntax.Location

class Kinded a where
    kindOf :: a -> Tc CorbenicKind

    -- replace any kind variables with ★
    -- will eventually be replaced with proper kind polymorphism
    defaultKind :: a -> Tc a

instance Kinded CorbenicKind where
    kindOf = zonkKind
    defaultKind k = do
        k' <- zonkKind k
        case k' of
            CKVar _    -> return CKStar
            CKArr a b  -> CKArr <$> defaultKind a <*> defaultKind b
            _          -> return k'

instance Kinded PrimType where
    kindOf PInteger = return CKStar
    kindOf PInteger32 = return CKStar
    kindOf PNatural = return CKStar
    kindOf PNatural32 = return CKStar
    kindOf PReal = return CKStar
    kindOf PRatio = return $ CKArr CKStar CKStar
    kindOf PBool = return CKStar
    kindOf PUnit = return CKStar
    kindOf PText = return CKStar
    kindOf PChar = return CKStar
    kindOf PFunction = return $ CKArr CKStar (CKArr CKStar CKStar) -- ★ → ★ → ★
    kindOf PIO = return $ CKArr CKStar CKStar
    kindOf PList = return $ CKArr CKStar CKStar

    defaultKind = return

instance Kinded TypeVar where
    kindOf = kindOf . tvKind
    defaultKind (TypeVar name sp k) = TypeVar name sp <$> defaultKind k

instance Kinded CorbenicType where
    kindOf (CTVar tv) = kindOf tv
    kindOf (CTCon sp i) = do
        ctors <- askFor tcTyCons
        case M.lookup i ctors of
            Just k -> zonkKind k
            Nothing -> throwError $ TypeCheckerError sp $ TCUnboundTypeConstructor i
    kindOf (CTFam sp _) = throwError $ TypeCheckerError sp $ TCNYI "Type Families"
    kindOf (CTPrim _ p) = kindOf p
    kindOf (CTApp sp f x) = do
        kf <- kindOf f
        kx <- kindOf x
        k2 <- freshKind
        unifyKind sp kf (CKArr kx k2)
        return k2
    kindOf (CTTuple _ _) = return CKStar
    kindOf (CTPred _ _) = return CKConstraint
    kindOf (CTForall{}) = return CKStar
    kindOf (CTExists{}) = return CKStar
    kindOf (CTConstrained _ _ t) = kindOf t


    defaultKind (CTVar tv) = CTVar <$> defaultKind tv
    defaultKind (CTCon sp i) = return $ CTCon sp i
    defaultKind (CTFam sp i) = return $ CTFam sp i
    defaultKind (CTPrim sp p) = return $ CTPrim sp p
    defaultKind (CTApp sp f x) = CTApp sp <$> defaultKind f <*> defaultKind x
    defaultKind (CTTuple sp ts) = CTTuple sp <$> mapM defaultKind ts
    defaultKind (CTPred sp ps) = CTPred sp <$> mapM defaultKind ps
    defaultKind (CTForall sp v t) = CTForall sp <$> defaultKind v <*> defaultKind t
    defaultKind (CTExists sp v t) = CTExists sp <$> defaultKind v <*> defaultKind t
    defaultKind (CTConstrained sp ps t) = CTConstrained sp <$> mapM defaultKind ps <*> defaultKind t
instance Kinded Pred where
    kindOf = const $ return CKConstraint

    defaultKind (Pred sp i t) = Pred sp i <$> mapM defaultKind t
    defaultKind (Equal sp a b) = Equal sp <$> defaultKind a <*> defaultKind b
    defaultKind (Quintessable sp a b) = Quintessable sp <$> defaultKind a <*> defaultKind b

instance Kinded Scheme where
    kindOf (Scheme _ _ ty) = kindOf ty
    defaultKind (Scheme tvs preds ty) = Scheme <$> mapM defaultKind tvs
                                               <*> mapM defaultKind preds
                                               <*> defaultKind ty


unifyKind :: Span -> CorbenicKind -> CorbenicKind -> Tc ()
unifyKind _ CKStar CKStar = pass
unifyKind _ CKConstraint CKConstraint = pass
unifyKind sp (CKArr a b) (CKArr c d) = unifyKind sp a c >> unifyKind sp b d
unifyKind sp (CKVar kv) k = bindKind sp kv k
unifyKind sp k (CKVar kv) = bindKind sp kv k
unifyKind sp a b = throwError $ TypeCheckerError sp $ TCKindMismatch a b

bindKind :: Span -> KindVar -> CorbenicKind -> Tc ()
bindKind sp kv k
  | k == CKVar kv = pass
  | kv `S.member` fkv k = throwError $ TypeCheckerError sp $ TCInfiniteKind kv k
  | otherwise = modify (over kindSubst (extendKSubst kv k))

zonkKind :: CorbenicKind -> Tc CorbenicKind
zonkKind k = do
    ks <- use kindSubst
    return $ applyK ks k
