module Frontend.TypeChecker.Seed (primitiveTermSeed, primitiveTypeSeed) where

import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location

-- the primitive type names
primitiveTypeSeed :: Map Identifier PrimType
primitiveTypeSeed =
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

-- most of these will be monomorphic. we only have polymorphic primitives if
-- the representation genuinely does not care about the type (e.g cons for lists)
-- so things like + should always be split primtype by primtype
primitiveTermSeed :: Map Text (Span -> Tc Scheme)
primitiveTermSeed =
    fromList
        [ (":", \sp -> freshMetavarTV sp CKStar >>= \a -> let la = mkList sp (CTVar a) in return (Scheme [a] [] (mkFun sp (CTVar a) (mkFun sp la la))))
        , ("+ℕ", \sp -> mono $ mkPFun2 sp PNatural PNatural PNatural)
        ]

mono :: CorbenicType -> Tc Scheme
mono t = return $ Scheme [] [] t

mkPFun :: Span -> PrimType -> PrimType -> CorbenicType
mkPFun sp a b = mkFun sp (CTPrim sp a) (CTPrim sp b)

mkPFun2 :: Span -> PrimType -> PrimType -> PrimType -> CorbenicType
mkPFun2 sp a b c = mkFun sp (CTPrim sp a) $ mkPFun sp b c
