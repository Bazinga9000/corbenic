module Frontend.TypeChecker.Error where

import Frontend.Diagnostics
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location

data TypeCheckerError = TypeCheckerError Span TypeCheckerErrorKind
    deriving (Eq, Ord, Show)

data TypeCheckerErrorKind
    = TCUnboundIdentifier Identifier
    | TCUnboundTypeConstructor Identifier
    | TCUnboundClass Identifier
    | TCCouldNotUnify CorbenicType CorbenicType
    | TCInfiniteType TypeVar CorbenicType
    | TCMissingInstance Pred
    | TCAmbiguousType CorbenicType
    | TCAmbiguousTypeVar TypeVar Pred
    | TCDuplicateDeclaration Identifier
    | TCSkolemEscape CorbenicType
    | TCBug Text
    | TCNYI Text
    deriving (Eq, Ord, Show)

instance Pretty TypeCheckerErrorKind where
    prettyPrint (TCUnboundIdentifier i) = "Unbound type-level identifier: " <> prettyPrint i
    prettyPrint (TCUnboundTypeConstructor ctor) = "Unbound type constructor: " <> prettyPrint ctor
    prettyPrint (TCUnboundClass cls) = "Unbound class: " <> prettyPrint cls
    prettyPrint (TCCouldNotUnify t1 t2) = "Could not unify: " <> prettyPrint t1 <> " and " <> prettyPrint t2
    prettyPrint (TCInfiniteType a b) = "Infinite type: " <> prettyPrint a <> " occurs in " <> prettyPrint b
    prettyPrint (TCMissingInstance predicate) = "Missing instance: " <> prettyPrint predicate
    prettyPrint (TCAmbiguousType t) = "Ambiguous type: " <> prettyPrint t
    prettyPrint (TCAmbiguousTypeVar tv predicate) = "Ambiguous type variable " <> prettyPrint tv <> " in " <> prettyPrint predicate
    prettyPrint (TCDuplicateDeclaration i) = "Duplicate declaration: " <> prettyPrint i
    prettyPrint (TCSkolemEscape tv) = "Skolem " <> prettyPrint tv <> " escaped its scope"
    prettyPrint (TCBug txt) = "Type checker bug (file a bug report at https://github.com/Bazinga9000/corbenic): " <> txt
    prettyPrint (TCNYI txt) = txt <> " are not yet implemented. Sorry!"

instance Pretty TypeCheckerError where
    prettyPrint (TypeCheckerError _ k) = prettyPrint k

instance Diagnosible TypeCheckerError where
    diagnose (TypeCheckerError s k) =
        Diagnostic
            { diagSeverity = SevError
            , diagMessage = prettyPrint k
            , diagSpan = s
            }

data TypeCheckerWarning = TypeCheckerWarning Span TypeCheckerWarningKind

data TypeCheckerWarningKind = TCOrphanInstance Pred

instance Pretty TypeCheckerWarningKind where
    prettyPrint (TCOrphanInstance predicate) = "Orphan instance: " <> prettyPrint predicate

instance Pretty TypeCheckerWarning where
    prettyPrint (TypeCheckerWarning _ k) = prettyPrint k

instance Diagnosible TypeCheckerWarning where
    diagnose (TypeCheckerWarning s k) =
        Diagnostic
            { diagSeverity = SevWarning
            , diagMessage = prettyPrint k
            , diagSpan = s
            }
