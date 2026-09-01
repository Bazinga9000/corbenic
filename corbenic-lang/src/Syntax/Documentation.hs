module Syntax.Documentation where

import Prelude

import Syntax.Identifier (Identifier)
import Syntax.Location (Span)

-- | A documentation comment attached to a declaration.
data DocComment = DocComment
    { docSpan :: Span
    , docLines :: [Text]
    }
    deriving (Eq, Show)

-- | Inline markup within a doc comment.
data Inline
    = InText Text
    | InCode Text
    | InRef Identifier
    | InBold [Inline]
    | InItalic [Inline]
    deriving (Eq, Show)
