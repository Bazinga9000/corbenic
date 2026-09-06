module Syntax.Location (
    Pos (..),
    Span (..),
    Located,
    Annotated (..),
    HasSpan (..),
    combine,
    combine3,
    combine4,
    combine5,
    combine6,
    combineAll,
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
type Located a = Annotated Span a

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

-- an annotated thing, regardless of its annotation type
data Annotated ann a = Annotated
    { annTag :: ann
    , annVal :: a
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class HasSpan a where
    spanOf :: a -> Span

instance HasSpan (Located x) where
    spanOf = annTag

instance HasSpan Span where
    spanOf = id

instance HasSpan (Span, a) where
    spanOf (s, _) = s

instance HasSpan (Span, a, b) where
    spanOf (s, _, _) = s

combine :: (HasSpan a, HasSpan b) => a -> b -> Span
combine a b = spanOf a <> spanOf b

combine3 :: (HasSpan a, HasSpan b, HasSpan c) => a -> b -> c -> Span
combine3 a b c = spanOf a <> spanOf b <> spanOf c

combine4 :: (HasSpan a, HasSpan b, HasSpan c, HasSpan d) => a -> b -> c -> d -> Span
combine4 a b c d = spanOf a <> spanOf b <> spanOf c <> spanOf d

combine5 :: (HasSpan a, HasSpan b, HasSpan c, HasSpan d, HasSpan e) => a -> b -> c -> d -> e -> Span
combine5 a b c d e = spanOf a <> spanOf b <> spanOf c <> spanOf d <> spanOf e

combine6 :: (HasSpan a, HasSpan b, HasSpan c, HasSpan d, HasSpan e, HasSpan f) => a -> b -> c -> d -> e -> f -> Span
combine6 a b c d e f = spanOf a <> spanOf b <> spanOf c <> spanOf d <> spanOf e <> spanOf f

combineAll :: (HasSpan a) => NonEmpty a -> Span
combineAll = sconcat . fmap spanOf

instance (Pretty a) => Pretty (Annotated ann a) where
    prettyPrint (Annotated _ a) = prettyPrint a
