module Syntax.Location (
    Pos (..),
    Span (..),
    Located (..),
) where

import Prelude

-- | A position in a source file
data Pos = Pos
    { posOffset :: Natural -- zero-indexed, counts code points
    , posLine :: Natural -- one-indexed
    , posCol :: Natural -- one-indexed
    }
    deriving (Eq, Show, Ord)

-- | A half-open source interval [start, end). A zero-width span is legal.
data Span = Span
    { spanStart :: Pos
    , spanEnd :: Pos
    }
    deriving (Eq, Show, Ord)

-- | A node tagged with its source span
data Located a = Located
    { locSpan :: Span
    , locAnn :: a
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup Span where
    -- this probably isn't necessary since spans are usually in order and disjoint
    -- but for paranoia we'll guarantee <> is commutative and always gets the right bounds
    (Span s1 e1) <> (Span s2 e2) = Span (min s1 s2) (max e1 e2)

instance Pretty Pos where
    prettyPrint p = toText (show (posLine p) <> ":" <> show (posCol p) :: String)

instance Pretty Span where
    prettyPrint (Span s e)
        | posLine s == posLine e = prettyPrint s <> "-" <> toText (show (posCol e) :: String)
        | otherwise = prettyPrint s <> "-" <> prettyPrint e

instance (Pretty a) => Pretty (Located a) where
    prettyPrint (Located _ a) = prettyPrint a
