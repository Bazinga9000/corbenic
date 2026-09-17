module Frontend.TypeChecker.Types where

import Data.Text qualified as T
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface
import Prelude

-- a corbenic kind.
data CorbenicKind
    = CKStar
    | CKConstraint -- constraints
    | CKArr CorbenicKind CorbenicKind
    | CKVar KindVar -- kind metavariable
    -- CKData -- user data (for later)
    deriving (Eq, Ord, Show)

-- a kind variable (just a natural)
newtype KindVar = KindVar Natural deriving (Eq, Ord, Show)

-- a type variable name (also just a natural)
-- either a normal metavar or a rigid/skolem (which can't be unificed)
data TypeVarName = Metavar Natural | Rigid Natural  deriving (Eq, Ord, Show)

-- a type variable, with its kind and the span where it was introduced.
data TypeVar = TypeVar
    { tvName :: TypeVarName
    , tvSpan :: Span
    , tvKind :: CorbenicKind
    }
    deriving (Show)

isRigid :: TypeVar -> Bool
isRigid (TypeVar (Rigid _) _ _) = True
isRigid _ = False

isMetavar :: TypeVar -> Bool
isMetavar = not . isRigid

instance Eq TypeVar where
    TypeVar n _ _ == TypeVar m _ _ = n == m

instance Ord TypeVar where
    compare (TypeVar n _ _) (TypeVar m _ _) = compare n m

-- a primitive type always in scope
data PrimType
    = PInteger -- ℤ
    | PInteger32 -- ℤ𐞄₃₂
    | PNatural -- ℕ
    | PNatural32 -- ℕ𐞄₃₂
    | PReal -- ℝ (internally 64 bit double)
    | PBool -- 𝔹
    | PUnit -- 𝟙
    | PText -- 𝕋
    | PChar -- 𝕔
    | PFunction -- →
    | PIO -- ⌘
    | PList -- ⎕ (also [])
    deriving (Eq, Ord, Show)

-- a realized corbenic type.
-- every node carries the span of the surface fragment it came from
-- (synthesized nodes carry the span of the expression being typed)
data CorbenicType
    = CTVar TypeVar -- type variable (either unification or skolem)
    | CTCon Span Identifier -- user type constructor
    | CTFam Span Identifier -- type family / associated type
    | CTPrim Span PrimType -- builtin primitive type
    | CTApp Span CorbenicType CorbenicType -- application
    | CTTuple Span [CorbenicType] -- (a, b, c)
    | CTPred Span [Pred] -- a constraint context in type position
    | CTForall Span TypeVar CorbenicType -- ∀x. τ
    | CTExists Span TypeVar CorbenicType -- ∃x. τ
    | CTConstrained Span [Pred] CorbenicType -- C a ⇒ τ (no quantification)
    deriving (Eq, Show, Ord)

-- a typeclass constraint (or a magical equivalent)
data Pred
    = Pred Span Identifier [CorbenicType] -- C a b
    | Equal Span CorbenicType CorbenicType -- a ~ b
    | Quintessable Span CorbenicType CorbenicType -- 🜀 a b
    deriving (Eq, Show, Ord)

-- a type scheme
data Scheme = Scheme [TypeVar] [Pred] CorbenicType
    deriving (Eq, Show)

-- a class declaration with methods and associated types
data ClassDef = ClassDef
    { clsName :: Identifier
    , clsArity :: Natural -- number of type params
    , clsSuper :: [Pred] -- superclass context (before ⇒)
    , clsMethods :: Map Identifier Scheme -- method schemes (class params in scope)
    , clsFamilies :: Map Identifier FamilyDef
    }

-- an associated type with optional default instantiation.
data FamilyDef = FamilyDef
    { famName :: Identifier
    , famArity :: Int
    , famDefault :: Maybe CorbenicType
    }

-- an instance with method impls and family equations
-- methods are stored typed (checked at instance-declaration time) so the
-- elaborator can build dictionaries from them directly.
data InstanceDef = InstanceDef
    { instHead :: Pred
    , instMethods :: Map Identifier (SurfaceExpr (Span, CorbenicType))
    , instFamilyEqns :: Map Identifier CorbenicType
    }

type ConstraintAlias = [Pred]

instance HasSpan TypeVar where
    spanOf = tvSpan

instance HasSpan CorbenicType where
    spanOf (CTVar tv) = spanOf tv
    spanOf (CTCon sp _) = sp
    spanOf (CTFam sp _) = sp
    spanOf (CTPrim sp _) = sp
    spanOf (CTApp sp _ _) = sp
    spanOf (CTTuple sp _) = sp
    spanOf (CTPred sp _) = sp
    spanOf (CTForall sp _ _) = sp
    spanOf (CTExists sp _ _) = sp
    spanOf (CTConstrained sp _ _) = sp

instance HasSpan Pred where
    spanOf (Pred sp _ _) = sp
    spanOf (Equal sp _ _) = sp
    spanOf (Quintessable sp _ _) = sp

instance Pretty TypeVarName where
    prettyPrint (Metavar n) = "t" <> mkSubscript n
    prettyPrint (Rigid n) = "rt" <> mkSubscript n

instance Pretty TypeVar where
    prettyPrint (TypeVar n _ _) = prettyPrint n

instance Pretty KindVar where
    prettyPrint (KindVar n) = "k" <> mkSubscript n

instance Pretty CorbenicKind where
    prettyPrint CKStar = "★"
    prettyPrint CKConstraint = "Constraint"
    prettyPrint (CKArr k1 k2) = parenKind k1 <> " → " <> prettyPrint k2 where
        parenKind k@(CKArr _ _) = "(" <> prettyPrint k <> ")"
        parenKind k = prettyPrint k
    prettyPrint (CKVar kv) = prettyPrint kv

instance Pretty PrimType where
    prettyPrint PInteger = "ℤ"
    prettyPrint PInteger32 = "ℤ𐞄₃₂"
    prettyPrint PNatural = "ℕ"
    prettyPrint PNatural32 = "ℕ𐞄₃₂"
    prettyPrint PReal = "ℝ"
    prettyPrint PBool = "𝔹"
    prettyPrint PUnit = "𝟙"
    prettyPrint PText = "𝕋"
    prettyPrint PChar = "𝕔"
    prettyPrint PFunction = "→"
    prettyPrint PIO = "⌘"
    prettyPrint PList = "⎕"

instance Pretty CorbenicType where
    prettyPrint (CTVar tv) = prettyPrint tv
    prettyPrint (CTCon _ i) = prettyPrint i
    prettyPrint (CTFam _ i) = prettyPrint i
    prettyPrint (CTPrim _ p) = prettyPrint p
    prettyPrint (CTApp _ (CTApp _ (CTPrim _ PFunction) a) b) = prettyPrint a <> " → " <> prettyPrint b
    prettyPrint (CTApp _ (CTPrim _ PList) a) = "[" <> prettyPrint a <> "]"
    prettyPrint (CTApp _ f x) = prettyPrint f <> " " <> atom x where
        atom (CTApp _ (CTPrim _ PList) a) = "[" <> prettyPrint a <> "]"
        atom t@(CTApp _ _ _) = "(" <> prettyPrint t <> ")"
        atom t@(CTTuple _ _) = "(" <> prettyPrint t <> ")"
        atom t@(CTForall _ _ _) = "(" <> prettyPrint t <> ")"
        atom t@(CTExists _ _ _) = "(" <> prettyPrint t <> ")"
        atom t@(CTConstrained _ _ _) = "(" <> prettyPrint t <> ")"
        atom t = prettyPrint t
    prettyPrint (CTTuple _ ts) = "(" <> T.intercalate ", " (map prettyPrint ts) <> ")"
    prettyPrint (CTPred _ ps) = T.intercalate ", " (map prettyPrint ps)
    prettyPrint (CTForall _ tv t) = "∀" <> prettyPrint tv <> ". " <> prettyPrint t
    prettyPrint (CTExists _ tv t) = "∃" <> prettyPrint tv <> ". " <> prettyPrint t
    prettyPrint (CTConstrained _ ps t) = T.intercalate ", " (map prettyPrint ps) <> " ⇒ " <> prettyPrint t

instance Pretty Pred where
    prettyPrint (Pred _ i args) = prettyPrint i <> " " <> T.intercalate " " (map prettyPrint args)
    prettyPrint (Equal _ a b) = prettyPrint a <> " ~ " <> prettyPrint b
    prettyPrint (Quintessable _ a b) = prettyPrint a <> " 🜀 " <> prettyPrint b
