module Syntax.Fixity where

import Syntax.Identifier
import Prelude

-- | A fixity declaration with its precedence information.
data Fixity
    = LeftAssocBinary Natural
    | RightAssocBinary Natural
    | NonAssocBinary Natural
    | PrefixUnary
    deriving (Eq, Show)

newtype FixityEnv = FixityEnv (Map Identifier Fixity)

instance Pretty Fixity where
    prettyPrint (LeftAssocBinary prec) = "⦿⌞" <> mkSubscript prec
    prettyPrint (RightAssocBinary prec) = "⦿⌟" <> mkSubscript prec
    prettyPrint (NonAssocBinary prec) = "⦿" <> mkSubscript prec
    prettyPrint PrefixUnary = "⦿⟓"
