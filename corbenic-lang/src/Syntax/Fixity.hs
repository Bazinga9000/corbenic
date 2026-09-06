module Syntax.Fixity where

import Syntax.Identifier
import Prelude

-- | A fixity declaration with its precedence information.
data Fixity
    = LeftAssocBinary Natural
    | RightAssocBinary Natural
    | NonAssocBinary Natural
    | PrefixUnary
    deriving (Eq, Ord, Show)

newtype FixityEnv = FixityEnv (Map Identifier Fixity)

instance Semigroup FixityEnv where
    FixityEnv a <> FixityEnv b = FixityEnv (a <> b)

instance Monoid FixityEnv where
    mempty = FixityEnv mempty

instance Pretty Fixity where
    prettyPrint (LeftAssocBinary prec) = "⦿⌞" <> mkSubscript prec
    prettyPrint (RightAssocBinary prec) = "⦿⌟" <> mkSubscript prec
    prettyPrint (NonAssocBinary prec) = "⦿" <> mkSubscript prec
    prettyPrint PrefixUnary = "⦿⟓"
