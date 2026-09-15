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
type Tc a = RWST TcEnv [Pred] TcState (Except TypeCheckerError) a

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

-- spawn a fresh typeVar (for use in metavariables)
freshTypeVar :: Span -> CorbenicKind -> Tc TypeVar
freshTypeVar sp k = do
    n <- use metaN
    metaN %= (+ 1)
    return $
        TypeVar
            { tvName = n
            , tvSpan = sp
            , tvKind = k
            }

-- spawn a frsh metavariable
freshMetavar :: Span -> CorbenicKind -> Tc CorbenicType
freshMetavar s k = CTVar <$> freshTypeVar s k

-- spawn a fresh rigid / skolem
freshRigid :: Span -> CorbenicKind -> Tc CorbenicType
freshRigid sp k = do
    n <- use rigidN
    rigidN %= (+ 1)
    return . CTRigid $
        TypeVar
            { tvName = n
            , tvSpan = sp
            , tvKind = k
            }

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
    tv <- freshTypeVar bsp k
    a <- local (\rho -> rho & tcTypeVars %~ M.insert ident tv) t
    return $ (tv, a)
