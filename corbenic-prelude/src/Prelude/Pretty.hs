{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Pretty where

import Data.Text qualified as T
import Relude
import Relude.Extra.Enum (safeToEnum)
import Data.Text (unpack)

-- | Canonical pretty-printer typeclass.
class Pretty a where
    prettyPrint :: a -> Text

instance Pretty Integer where
    prettyPrint = show

instance Pretty Text where
    prettyPrint = id

instance (Pretty a) => Pretty [a] where
    prettyPrint xs = "[" <> T.intercalate ", " (map prettyPrint xs) <> "]"

-- | Render a natural number as a run of subscript digits.
mkSubscript :: Natural -> Text
mkSubscript n = toText (map digit (show n :: String))
  where
    digit :: Char -> Char
    digit c = fromMaybe c (safeToEnum (fromEnum '₀' + fromEnum c - fromEnum '0'))

-- | Convert a sequence of subscripted digits into the natural it represents
unSubscript :: Text -> Maybe Natural
unSubscript "₀" = Just 0
unSubscript txt = if leadingZero txt' then Nothing else go (reverse txt') where
  txt' = unpack txt

  leadingZero ('₀':_) = True
  leadingZero _ = False

  go [] = Just 0
  go ('₁':t) = (\x -> 10*x + 1) <$> go t
  go ('₂':t) = (\x -> 10*x + 2) <$> go t
  go ('₃':t) = (\x -> 10*x + 3) <$> go t
  go ('₄':t) = (\x -> 10*x + 4) <$> go t
  go ('₅':t) = (\x -> 10*x + 5) <$> go t
  go ('₆':t) = (\x -> 10*x + 6) <$> go t
  go ('₇':t) = (\x -> 10*x + 7) <$> go t
  go ('₈':t) = (\x -> 10*x + 8) <$> go t
  go ('₉':t) = (\x -> 10*x + 9) <$> go t
  go _ = Nothing
