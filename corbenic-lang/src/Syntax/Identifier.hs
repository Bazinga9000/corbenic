module Syntax.Identifier where

import Prelude

-- | A Corbenic identifier.
data Identifier
    = IdentRaw Text -- normal (e.g "x" -> Raw "x")
    | IdentQuoted Text -- quoted with guillemets (e.g "«x»" -> Quoted "x")
    | IdentPrimitive Text -- quoted with ornate brackets (e.g "﴾x﴿"-> Primitive "x")
    | IdentType Text -- quoted with type variable brackets (e.g "〈x〉" -> Type "x")
    | IdentGenerated Natural -- internally-generated identifiers
    deriving (Show)

instance Eq Identifier where
    -- raw and quoted identifiers are the same
    IdentRaw a == IdentQuoted b = a == b
    IdentQuoted a == IdentRaw b = a == b
    -- everything else is just functorial
    IdentRaw a == IdentRaw b = a == b
    IdentQuoted a == IdentQuoted b = a == b
    IdentPrimitive a == IdentPrimitive b = a == b
    IdentType a == IdentType b = a == b
    IdentGenerated a == IdentGenerated b = a == b
    _ == _ = False

instance Ord Identifier where
    compare = comparing identifierText
      where
        identifierText :: Identifier -> (Natural, Text)
        identifierText (IdentRaw t) = (0, t)
        identifierText (IdentQuoted t) = (0, t)
        identifierText (IdentPrimitive t) = (1, t)
        identifierText (IdentType t) = (2, t)
        identifierText (IdentGenerated n) = (3, show n)

instance Pretty Identifier where
    prettyPrint (IdentRaw t) = t
    prettyPrint (IdentQuoted t) = "«" <> t <> "»"
    prettyPrint (IdentPrimitive t) = "﴾" <> t <> "﴿"
    prettyPrint (IdentType t) = "〈" <> t <> "〉"
    prettyPrint (IdentGenerated n) = "﴾g#" <> show n <> "﴿"
