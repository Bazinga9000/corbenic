module Syntax.Literal where

import Prelude

-- | A Corbenic Literal. These will mostly have very overloaded types.
data Literal
    = LitNatural Natural
    | LitRational Rational
    | LitBool Bool
    | LitChar Char
    | LitText Text
    deriving (Eq, Show)

instance Pretty Literal where
    prettyPrint (LitNatural n) = show n
    prettyPrint (LitRational x) = show x
    prettyPrint (LitBool b) = if b then "⊤" else "⊥"
    prettyPrint (LitChar c) = "'" <> one c <> "'"
    prettyPrint (LitText t) = show t
