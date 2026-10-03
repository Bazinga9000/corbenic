module Frontend.TypeChecker.Seed where

import Frontend.TypeChecker.Types
import Syntax.Identifier

-- the primitive type names
primitiveSeed :: Map Identifier PrimType
primitiveSeed =
    fromList
        [ (IdentRaw "ℤ", PInteger)
        , (IdentQuoted "ℤ𐞄₃₂", PInteger32)
        , (IdentRaw "ℕ", PNatural)
        , (IdentQuoted "ℕ𐞄₃₂", PNatural32)
        , (IdentRaw "ℝ", PReal)
        , (IdentRaw "⫽", PRatio)
        , (IdentRaw "𝔹", PBool)
        , (IdentRaw "𝟙", PUnit)
        , (IdentRaw "𝕋", PText)
        , (IdentRaw "𝕔", PChar)
        , (IdentRaw "→", PFunction)
        , (IdentRaw "⌘", PIO)
        , (IdentRaw "⎕", PList)
        ]
