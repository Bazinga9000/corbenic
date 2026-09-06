module Syntax.Surface where

import Syntax.Documentation
import Syntax.Fixity
import Syntax.Literal
import Syntax.Location (Annotated (..))

data SurfaceName
    = SNRaw Text
    | SNQuoted Text
    | SNPrimitive Text
    deriving (Show)

data SurfaceDeclName
    = SDNRaw Text
    | SDNQuoted Text
    deriving (Show)

data SurfaceModule ann = SurfaceModule
    { smName :: ModulePath ann
    , smDoc :: Maybe DocComment -- module-wide doc comment
    , smImports :: [SurfaceImport ann]
    , smDecls :: [MaybeExported SurfaceDeclaration ann]
    , smTermFixity :: FixityEnv
    , smTypeFixity :: FixityEnv
    }

data MaybeExported thing ann = Exported (thing ann) | Hidden (thing ann)

type ModulePath ann = NonEmpty (Annotated ann SurfaceName)

data SurfaceImport ann
    = SurfaceImport ann (ModulePath ann) (Maybe Qualifier) (Maybe (ImportSpec ann)) -- ⇲ Foo.Bar [⌸ [T]] [block]
    | SurfaceReexport ann (ModulePath ann) (ImportSpec ann) -- ⇱ Foo.Bar [block]

data Qualifier = ExpliitQualifier SurfaceName | ImplicitQualifier

data ImportSpec ann
    = ImportAll
    | ImportSelect [Annotated ann SurfaceName] [Annotated ann SurfaceName]

-- things explicitly not here that might be surprising:
-- SIf - conditionals are done by the `?` glyph, which is `Bool -> a -> a -> a`
-- SLet - the preferred abstraction for binding is the where block
data SurfaceExpr ann
    = SELiteral ann Literal -- 2
    | SEIdentifier (Annotated ann SurfaceName) -- x
    | SELambda ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- λx ↦ foo
    | SEApp ann (SurfaceExpr ann) (SurfaceExpr ann) -- f x
    | SETypeLambda ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- Λx ↦ foo
    | SETypeApp ann (SurfaceExpr ann) (SurfaceType ann) -- f 〈x〉
    | SEWhere ann (SurfaceExpr ann) [SurfaceWhereDeclaration ann] -- expr with a where block
    | SEDo ann [SurfaceDoInstruction ann] (SurfaceExpr ann) -- 🝣 \n foo ≔ bar \n baz ↤ quux \n bep
    | SECase ann (SurfaceExpr ann) [SurfaceBranch ann] -- 🝡x \n <pattern match>
    | SELambdaCase ann [SurfaceBranch ann] -- λ🝡 \n <pattern match>
    | SEList ann [SurfaceExpr ann] -- [foo, bar, baz]
    | SETuple ann [SurfaceExpr ann] -- (foo, bar, baz)
    | SEOpSectionL ann (SurfaceExpr ann) (Annotated ann SurfaceName) -- (3+)
    | SEOpSectionR ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- (+3)
    | SEInfix ann (SurfaceExpr ann) (Annotated ann SurfaceName) (SurfaceExpr ann) -- 2+2
    | SEAnnotation ann (SurfaceExpr ann) (SurfaceType ann) -- inline type annotation
    | SEHole ann

data SurfaceBranch ann = SurfaceBranch
    { sbAnn :: ann
    , sbPattern :: SurfacePattern ann
    , sbBody :: SurfaceExpr ann
    }

data SurfacePattern ann
    = SPLiteral ann Literal -- 0
    | SPVar (Annotated ann SurfaceName) -- x
    | SPCon ann (Annotated ann SurfaceName) [SurfacePattern ann] -- Just x
    | SPTuple ann [SurfacePattern ann] -- (a, b, c)
    | SPList ann [SurfacePattern ann] -- [a, b, c]
    | SPWild ann -- _

data SurfaceType ann
    = STName (Annotated ann SurfaceName)
    | STApp ann (SurfaceType ann) (SurfaceType ann)
    | STFun ann (SurfaceType ann) (SurfaceType ann)
    | STList ann (SurfaceType ann) -- [a]
    | STTuple ann [SurfaceType ann] -- (a, b, c)
    | STConstraint ann (SurfaceClassContext ann)
    | STForall ann [Annotated ann SurfaceName] (SurfaceType ann)
    | STExists ann [Annotated ann SurfaceName] (SurfaceType ann)

data SurfaceTypeConstructor ann = SurfaceTypeConstructor
    { stcAnn :: ann
    , stcDoc :: DocComment
    , stcName :: Annotated ann SurfaceName
    , stcFields :: [SurfaceType ann]
    }

data SurfaceDoInstruction ann
    = SDIBindName ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- x ≔ y
    | SDIExtractMonad ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- x ↤ y
    | SDIMonadicStmt ann (SurfaceExpr ann) -- x

-- individual types of declarations
data SurfaceTermDecl ann = SurfaceTermDecl
    { stdAnn :: ann
    , stdDoc :: DocComment
    , stdName :: Annotated ann SurfaceDeclName
    , stdBody :: SurfaceExpr ann
    }

data SurfaceTypeDecl ann = SurfaceTypeDecl
    { stydAnn :: ann
    , stydDoc :: DocComment
    , stydName :: Annotated ann SurfaceDeclName
    , stydType :: SurfaceType ann
    }

data SurfaceDataDecl ann = SurfaceDataDecl
    { sddAnn :: ann
    , sddDoc :: DocComment
    , sddName :: Annotated ann SurfaceDeclName
    , sddParams :: [Annotated ann SurfaceName]
    , sddConstructors :: [MaybeExported SurfaceTypeConstructor ann]
    }

data SurfaceNewtypeDecl ann = SurfaceNewtypeDecl
    { sndAnn :: ann
    , sndDoc :: DocComment
    , sndName :: Annotated ann SurfaceDeclName
    , sndParams :: [Annotated ann SurfaceName]
    , sndConstructor :: MaybeExported SurfaceTypeConstructor ann
    }

data SurfaceTypeAlias ann = SurfaceTypeAlias
    { staAnn :: ann
    , staDoc :: DocComment
    , staName :: Annotated ann SurfaceDeclName
    , staParams :: [Annotated ann SurfaceName]
    , staBody :: SurfaceType ann
    }

data SurfaceConstraintAlias ann = SurfaceConstraintAlias
    { scaAnn :: ann
    , scaDoc :: DocComment
    , scaHead :: SurfaceClassApp ann
    , scaBody :: SurfaceClassContext ann
    }

data SurfaceAssociatedType ann = SurfaceAssociatedType
    { satAnn :: ann
    , satDoc :: DocComment
    , satName :: Annotated ann SurfaceDeclName
    , satParams :: [Annotated ann SurfaceName]
    , satBody :: Maybe (SurfaceType ann)
    }

-- declaration contexts (allow certain subsets of them)
data SurfaceClassMember ann
    = SCMethod (SurfaceTypeDecl ann) (Maybe (SurfaceTermDecl ann))
    | SCAssociatedType (SurfaceAssociatedType ann)

data SurfaceWhereDeclaration ann
    = SWDTerm (Maybe (SurfaceTypeDecl ann)) (SurfaceTermDecl ann)
    | SWDType (SurfaceTypeDecl ann)

data SurfaceDeclaration ann
    = SDTerm (Maybe (SurfaceTypeDecl ann)) (SurfaceTermDecl ann)
    | SDClass (SurfaceClassDecl ann)
    | SDInstance (SurfaceInstanceDecl ann)
    | SDData (SurfaceDataDecl ann)
    | SDNewtype (SurfaceNewtypeDecl ann)
    | SDTypeAlias (SurfaceTypeAlias ann)
    | SDConstraintAlias (SurfaceConstraintAlias ann)

-- typeclass definitions
data SurfaceClassDecl ann = SurfaceClassDecl
    { scdAnn :: ann
    , scdDoc :: DocComment
    , scdSuper :: Maybe (SurfaceClassContext ann) -- constraints before ⇒
    , scdHead :: SurfaceClassApp ann -- C a b after ⇒
    , scdMembers :: [SurfaceClassMember ann] -- the block
    }

data SurfaceClassApp ann = SurfaceClassApp
    { scappAnn :: ann
    , scappName :: Annotated ann SurfaceName
    , scappArgs :: [SurfaceType ann]
    } -- Foo a b c ...

data SurfaceInstanceDecl ann = SurfaceInstanceDecl
    { sidAnn :: ann
    , sidDoc :: DocComment
    , sidHead :: SurfaceClassApp ann
    , sidMembers :: [SurfaceInstanceMember ann]
    }

data SurfaceInstanceMember ann
    = SIMethod (SurfaceTermDecl ann)
    | SIMAssociatedType (SurfaceTypeAlias ann)

data SurfaceClassContext ann = SurfaceClassContext
    { sctxAnn :: ann
    , sctxApps :: [SurfaceClassApp ann]
    } -- (Foo a b, Bar c d, Baz e f, ...)
