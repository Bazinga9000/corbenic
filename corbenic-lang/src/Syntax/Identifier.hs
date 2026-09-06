module Syntax.Identifier where

import Prelude

-- | A Corbenic identifier.
data Identifier
    = IdentRaw Text -- normal (e.g "x" -> Raw "x")
    | IdentQuoted Text -- quoted with guillemets (e.g "«x»" -> Quoted "x")
    | IdentPrimitive Text -- quoted with ornate brackets (e.g "﴾x﴿"-> Primitive "x")
    | IdentGenerated Natural -- internally-generated identifiers
    deriving (Show)

-- | A Corbenic identifier, with an **annotation**
data AnnotatedIdent ann = AnnotatedIdent ann Identifier

instance Eq Identifier where
    -- raw and quoted identifiers are the same
    IdentRaw a == IdentQuoted b = a == b
    IdentQuoted a == IdentRaw b = a == b
    -- everything else is just functorial
    IdentRaw a == IdentRaw b = a == b
    IdentQuoted a == IdentQuoted b = a == b
    IdentPrimitive a == IdentPrimitive b = a == b
    IdentGenerated a == IdentGenerated b = a == b
    _ == _ = False

instance Ord Identifier where
    compare = comparing identifierText
      where
        identifierText :: Identifier -> (Natural, Text)
        identifierText (IdentRaw t) = (0, t)
        identifierText (IdentQuoted t) = (0, t)
        identifierText (IdentPrimitive t) = (1, t)
        identifierText (IdentGenerated n) = (2, show n)

instance Pretty Identifier where
    prettyPrint (IdentRaw t) = t
    prettyPrint (IdentQuoted t) = "«" <> t <> "»"
    prettyPrint (IdentPrimitive t) = "﴾" <> t <> "﴿"
    prettyPrint (IdentGenerated n) = "﴾g#" <> show n <> "﴿"
