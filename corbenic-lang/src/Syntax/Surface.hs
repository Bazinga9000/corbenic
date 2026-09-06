module Syntax.Surface where

import Syntax.Documentation
import Syntax.Fixity
import Syntax.Identifier (Identifier)
import Syntax.Literal
import Syntax.Location

data SurfaceModule ann = SurfaceModule
    { smName :: ModulePath ann
    , smDoc :: Maybe DocComment -- module-wide doc comment
    , smImports :: [SurfaceImport ann]
    , smDecls :: [MaybeExported SurfaceDeclaration ann]
    , smTermFixity :: FixityEnv
    , smTypeFixity :: FixityEnv
    }

data MaybeExported thing ann
    = Exported Span (thing ann)
    | Hidden Span (thing ann)

type ModulePath ann = NonEmpty (Annotated ann Identifier)

data SurfaceImport ann
    = SurfaceImport ann (ModulePath ann) (Maybe Qualifier) (Maybe (ImportSpec ann)) -- ⇲ Foo.Bar [⌸ [T]] [block]
    | SurfaceReexport ann (ModulePath ann) (ImportSpec ann) -- ⇱ Foo.Bar [block]

data Qualifier = ExpliitQualifier Identifier | ImplicitQualifier

data ImportSpec ann
    = ImportAll
    | ImportSelect [Annotated ann Identifier] [Annotated ann Identifier]

-- things explicitly not here that might be surprising:
-- SIf - conditionals are done by the `?` glyph, which is `Bool -> a -> a -> a`
-- SLet - the preferred abstraction for binding is the where block
data SurfaceExpr ann
    = SELiteral ann Literal -- 2
    | SEIdentifier (Annotated ann Identifier) -- x
    | SELambda ann (Annotated ann Identifier) (SurfaceExpr ann) -- λx ↦ foo
    | SEApp ann (SurfaceExpr ann) (SurfaceExpr ann) -- f x
    | SETypeLambda ann (Annotated ann Identifier) (SurfaceExpr ann) -- Λx ↦ foo
    | SETypeApp ann (SurfaceExpr ann) (SurfaceType ann) -- f 〈x〉
    | SEWhere ann (SurfaceExpr ann) [SurfaceWhereDeclaration ann] -- expr with a where block
    | SEDo ann [SurfaceDoInstruction ann] (SurfaceExpr ann) -- 🝣 \n foo ≔ bar \n baz ↤ quux \n bep
    | SECase ann (SurfaceExpr ann) [SurfaceBranch ann] -- 🝡x \n <pattern match>
    | SELambdaCase ann [SurfaceBranch ann] -- λ🝡 \n <pattern match>
    | SEList ann [SurfaceExpr ann] -- [foo, bar, baz]
    | SETuple ann [SurfaceExpr ann] -- (foo, bar, baz)
    | SEOpSectionL ann (SurfaceExpr ann) (Annotated ann Identifier) -- (3+)
    | SEOpSectionR ann (Annotated ann Identifier) (SurfaceExpr ann) -- (+3)
    | SEInfix ann (SurfaceExpr ann) (Annotated ann Identifier) (SurfaceExpr ann) -- 2+2
    | SEAnnotation ann (SurfaceExpr ann) (SurfaceType ann) -- inline type annotation
    | SEHole ann

data SurfaceBranch ann = SurfaceBranch
    { sbAnn :: ann
    , sbPattern :: SurfacePattern ann
    , sbBody :: SurfaceExpr ann
    }

data SurfacePattern ann
    = SPLiteral ann Literal -- 0
    | SPVar (Annotated ann Identifier) -- x
    | SPCon ann (Annotated ann Identifier) [SurfacePattern ann] -- Just x
    | SPTuple ann [SurfacePattern ann] -- (a, b, c)
    | SPList ann [SurfacePattern ann] -- [a, b, c]
    | SPWild ann -- _

data SurfaceType ann
    = STName (Annotated ann Identifier)
    | STApp ann (SurfaceType ann) (SurfaceType ann)
    | STFun ann (SurfaceType ann) (SurfaceType ann)
    | STList ann (SurfaceType ann) -- [a]
    | STTuple ann [SurfaceType ann] -- (a, b, c)
    | STConstraint ann (SurfaceClassContext ann)
    | STForall ann [Annotated ann Identifier] (SurfaceType ann)
    | STExists ann [Annotated ann Identifier] (SurfaceType ann)

data SurfaceTypeConstructor ann = SurfaceTypeConstructor
    { stcAnn :: ann
    , stcDoc :: DocComment
    , stcName :: Annotated ann Identifier
    , stcFields :: [SurfaceType ann]
    }

data SurfaceDoInstruction ann
    = SDIBindName ann (Annotated ann Identifier) (SurfaceExpr ann) -- x ≔ y
    | SDIExtractMonad ann (Annotated ann Identifier) (SurfaceExpr ann) -- x ↤ y
    | SDIMonadicStmt ann (SurfaceExpr ann) -- x

-- individual types of declarations
data SurfaceTermDecl ann = SurfaceTermDecl
    { stdAnn :: ann
    , stdDoc :: DocComment
    , stdName :: Annotated ann Identifier
    , stdBody :: SurfaceExpr ann
    }

data SurfaceTypeDecl ann = SurfaceTypeDecl
    { stydAnn :: ann
    , stydDoc :: DocComment
    , stydName :: Annotated ann Identifier
    , stydType :: SurfaceType ann
    }

data SurfaceDataDecl ann = SurfaceDataDecl
    { sddAnn :: ann
    , sddDoc :: DocComment
    , sddName :: Annotated ann Identifier
    , sddParams :: [Annotated ann Identifier]
    , sddConstructors :: [MaybeExported SurfaceTypeConstructor ann]
    }

data SurfaceNewtypeDecl ann = SurfaceNewtypeDecl
    { sndAnn :: ann
    , sndDoc :: DocComment
    , sndName :: Annotated ann Identifier
    , sndParams :: [Annotated ann Identifier]
    , sndConstructor :: MaybeExported SurfaceTypeConstructor ann
    }

data SurfaceTypeAlias ann = SurfaceTypeAlias
    { staAnn :: ann
    , staDoc :: DocComment
    , staName :: Annotated ann Identifier
    , staParams :: [Annotated ann Identifier]
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
    , satName :: Annotated ann Identifier
    , satParams :: [Annotated ann Identifier]
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
    , scappName :: Annotated ann Identifier
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

-- HasSpan instances ---------------------------------------------------------

instance (HasSpan ann) => HasSpan (SurfaceExpr ann) where
    spanOf (SELiteral ann _) = spanOf ann
    spanOf (SEIdentifier (Annotated ann _)) = spanOf ann
    spanOf (SELambda ann _ _) = spanOf ann
    spanOf (SEApp ann _ _) = spanOf ann
    spanOf (SETypeLambda ann _ _) = spanOf ann
    spanOf (SETypeApp ann _ _) = spanOf ann
    spanOf (SEWhere ann _ _) = spanOf ann
    spanOf (SEDo ann _ _) = spanOf ann
    spanOf (SECase ann _ _) = spanOf ann
    spanOf (SELambdaCase ann _) = spanOf ann
    spanOf (SEList ann _) = spanOf ann
    spanOf (SETuple ann _) = spanOf ann
    spanOf (SEOpSectionL ann _ _) = spanOf ann
    spanOf (SEOpSectionR ann _ _) = spanOf ann
    spanOf (SEInfix ann _ _ _) = spanOf ann
    spanOf (SEAnnotation ann _ _) = spanOf ann
    spanOf (SEHole ann) = spanOf ann

instance (HasSpan ann) => HasSpan (SurfacePattern ann) where
    spanOf (SPLiteral ann _) = spanOf ann
    spanOf (SPVar (Annotated ann _)) = spanOf ann
    spanOf (SPCon ann _ _) = spanOf ann
    spanOf (SPTuple ann _) = spanOf ann
    spanOf (SPList ann _) = spanOf ann
    spanOf (SPWild ann) = spanOf ann

instance (HasSpan ann) => HasSpan (SurfaceType ann) where
    spanOf (STName (Annotated ann _)) = spanOf ann
    spanOf (STApp ann _ _) = spanOf ann
    spanOf (STFun ann _ _) = spanOf ann
    spanOf (STList ann _) = spanOf ann
    spanOf (STTuple ann _) = spanOf ann
    spanOf (STConstraint ann _) = spanOf ann
    spanOf (STForall ann _ _) = spanOf ann
    spanOf (STExists ann _ _) = spanOf ann

instance (HasSpan ann) => HasSpan (SurfaceDoInstruction ann) where
    spanOf (SDIBindName ann _ _) = spanOf ann
    spanOf (SDIExtractMonad ann _ _) = spanOf ann
    spanOf (SDIMonadicStmt ann _) = spanOf ann

instance (HasSpan ann) => HasSpan (SurfaceBranch ann) where
    spanOf = spanOf . sbAnn

instance (HasSpan ann) => HasSpan (SurfaceTypeConstructor ann) where
    spanOf = spanOf . stcAnn

instance (HasSpan ann) => HasSpan (SurfaceTermDecl ann) where
    spanOf = spanOf . stdAnn

instance (HasSpan ann) => HasSpan (SurfaceTypeDecl ann) where
    spanOf = spanOf . stydAnn

instance (HasSpan ann) => HasSpan (SurfaceDataDecl ann) where
    spanOf = spanOf . sddAnn

instance (HasSpan ann) => HasSpan (SurfaceNewtypeDecl ann) where
    spanOf = spanOf . sndAnn

instance (HasSpan ann) => HasSpan (SurfaceTypeAlias ann) where
    spanOf = spanOf . staAnn

instance (HasSpan ann) => HasSpan (SurfaceConstraintAlias ann) where
    spanOf = spanOf . scaAnn

instance (HasSpan ann) => HasSpan (SurfaceAssociatedType ann) where
    spanOf = spanOf . satAnn

instance (HasSpan ann) => HasSpan (SurfaceClassMember ann) where
    spanOf (SCMethod d _) = spanOf d
    spanOf (SCAssociatedType d) = spanOf d

instance (HasSpan ann) => HasSpan (SurfaceWhereDeclaration ann) where
    spanOf (SWDTerm _ d) = spanOf d
    spanOf (SWDType d) = spanOf d

instance (HasSpan ann) => HasSpan (SurfaceDeclaration ann) where
    spanOf (SDTerm _ d) = spanOf d
    spanOf (SDClass d) = spanOf d
    spanOf (SDInstance d) = spanOf d
    spanOf (SDData d) = spanOf d
    spanOf (SDNewtype d) = spanOf d
    spanOf (SDTypeAlias d) = spanOf d
    spanOf (SDConstraintAlias d) = spanOf d

instance (HasSpan ann) => HasSpan (SurfaceClassDecl ann) where
    spanOf = spanOf . scdAnn

instance (HasSpan ann) => HasSpan (SurfaceClassApp ann) where
    spanOf = spanOf . scappAnn

instance (HasSpan ann) => HasSpan (SurfaceInstanceDecl ann) where
    spanOf = spanOf . sidAnn

instance (HasSpan ann) => HasSpan (SurfaceInstanceMember ann) where
    spanOf (SIMethod d) = spanOf d
    spanOf (SIMAssociatedType d) = spanOf d

instance (HasSpan ann) => HasSpan (SurfaceClassContext ann) where
    spanOf = spanOf . sctxAnn

instance (HasSpan ann) => HasSpan (SurfaceImport ann) where
    spanOf (SurfaceImport ann _ _ _) = spanOf ann
    spanOf (SurfaceReexport ann _ _) = spanOf ann
