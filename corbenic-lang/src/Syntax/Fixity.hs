module Syntax.Fixity (
    Fixity (..),
    FixityEnv (..),
    lookupFixity,
    isBinary,
) where

import Data.Map.Strict qualified as Map
import Syntax.Identifier
import Prelude

-- | A fixity declaration with its precedence information.
data Fixity
    = LeftAssocBinary Natural
    | RightAssocBinary Natural
    | NonAssocBinary Natural
    | PrefixUnary Natural
    | PostfixUnary Natural
    deriving (Eq, Ord, Show)

newtype FixityEnv = FixityEnv (Map Identifier Fixity)

lookupFixity :: Identifier -> FixityEnv -> Maybe Fixity
lookupFixity name (FixityEnv m) = Map.lookup name m

isBinary :: Fixity -> Bool
isBinary (PrefixUnary _) = False
isBinary (PostfixUnary _) = False
isBinary _ = True

instance Semigroup FixityEnv where
    FixityEnv a <> FixityEnv b = FixityEnv (a <> b)

instance Monoid FixityEnv where
    mempty = FixityEnv mempty

instance Pretty Fixity where
    prettyPrint (LeftAssocBinary prec) = "⦿⌞" <> mkSubscript prec
    prettyPrint (RightAssocBinary prec) = "⦿⌟" <> mkSubscript prec
    prettyPrint (NonAssocBinary prec) = "⦿" <> mkSubscript prec
    prettyPrint (PrefixUnary prec) = "⦿⟓" <> mkSubscript prec
    prettyPrint (PostfixUnary prec) = "⦿Ŀ" <> mkSubscript prec
