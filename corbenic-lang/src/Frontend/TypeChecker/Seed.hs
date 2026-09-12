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
        , (IdentRaw "𝔹", PBool)
        , (IdentRaw "𝟙", PUnit)
        , (IdentRaw "𝕋", PText)
        , (IdentRaw "𝕔", PChar)
        , (IdentRaw "→", PFunction)
        , (IdentRaw "⌘", PIO)
        , (IdentRaw "⎕", PList)
        ]

-- the kind of a primitive
primKind :: PrimType -> CorbenicKind
primKind PInteger = CKStar
primKind PInteger32 = CKStar
primKind PNatural = CKStar
primKind PNatural32 = CKStar
primKind PReal = CKStar
primKind PBool = CKStar
primKind PUnit = CKStar
primKind PText = CKStar
primKind PChar = CKStar
primKind PFunction = CKArr CKStar (CKArr CKStar CKStar) -- ★ → ★ → ★
primKind PIO = CKArr CKStar CKStar
primKind PList = CKArr CKStar CKStar
