module Frontend.TypeChecker.Tc where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Frontend.Flags
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Subst
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location
import Data.Map qualified as M

-- the type checker monad
type Tc a = RWST TcEnv TcWriter TcState (Except TypeCheckerError) a

-- the type checker **writer**
-- logs warnings, also logs constraints for later solving
data TcWriter = TcWriter
    { twPreds :: [Pred]
    , twWarnings :: [TypeCheckerWarning]
    }

instance Semigroup TcWriter where
    TcWriter p1 w1 <> TcWriter p2 w2 = TcWriter (p1 <> p2) (w1 <> w2)

instance Monoid TcWriter where
    mempty = TcWriter [] []

tellPreds :: [Pred] -> Tc ()
tellPreds ps = tell (TcWriter ps [])

warn :: TypeCheckerWarning -> Tc ()
warn w = tell (TcWriter [] [w])

-- clear only the constraints (for the letrec/generalize pattern), keeping warnings
censorPreds :: Tc a -> Tc a
censorPreds = censor (\w -> w { twPreds = [] })

-- the type checker **state**
data TcState = TcState
    { _metaN :: Natural
    , _rigidN :: Natural
    , _kindN :: Natural
    , _currentSubst :: Subst
    }

-- the type checker **environment**
data TcEnv = TcEnv
    { _tcTerms :: Map Identifier Scheme -- term values (incl. constructors)
    , _tcTyCons :: Map Identifier CorbenicKind -- type constructors in scope
    , _tcClasses :: Map Identifier ClassDef
    , _tcInstances :: Map Identifier [InstanceDef] -- by class name
    , _tcAliases :: Map Identifier ([TypeVar], CorbenicType) -- type aliases (expanded on use)
    , _tcConstraintAliases :: Map Identifier ConstraintAlias
    , _tcTypeVars :: Map Identifier TypeVar -- locally bound type variables
    , _flags :: FrontendFlags
    }

makeLenses ''TcState
makeLenses ''TcEnv

-- generate a fresh name by incrementing a lens and applying a ctor
freshName :: (Natural -> TypeVarName) -> (Lens' TcState Natural) -> Tc TypeVarName
freshName ctor l = ctor <$> ((l %= (+1)) *> use l)

-- spawn a fresh typeVar
freshVar :: (Natural -> TypeVarName) -> (Lens' TcState Natural) -> Span -> CorbenicKind -> Tc TypeVar
freshVar ctor l sp k = do
    mn <- freshName ctor l
    return $
        TypeVar
            { tvName = mn
            , tvSpan = sp
            , tvKind = k
            }

-- spawn a fresh metavariable TVar
freshMetavarTV :: Span -> CorbenicKind -> Tc TypeVar
freshMetavarTV sp k = freshVar Metavar metaN sp k

-- spawn a fresh metavariable CorbenicType
freshMetavar :: Span -> CorbenicKind -> Tc CorbenicType
freshMetavar sp k = CTVar <$> freshMetavarTV sp k

-- spawn a fresh rigid / skolem CorbenicType
freshRigid :: Span -> CorbenicKind -> Tc CorbenicType
freshRigid sp k = CTVar <$> freshVar Rigid rigidN sp k

-- spawn a fresh kind variable
freshKind :: Tc CorbenicKind
freshKind = do
    n <- use kindN
    kindN %= (+ 1)
    return . CKVar . KindVar $ n

-- locally bind an identifier to a fresh type variable within a given typechecker computation
bindToTVar :: Annotated Span Identifier -> Tc a -> Tc (TypeVar, a)
bindToTVar (Annotated bsp ident) t = do
    k <- freshKind
    tv <- freshMetavarTV bsp k
    a <- local (\rho -> rho & tcTypeVars %~ M.insert ident tv) t
    return $ (tv, a)
