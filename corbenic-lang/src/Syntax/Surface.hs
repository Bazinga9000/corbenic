module Syntax.Surface where

import Syntax.Documentation
import Syntax.Fixity
import Syntax.Identifier
import Syntax.Literal

data SurfaceModule ann = SurfaceModule
    { smName :: (AnnotatedIdent ann)
    , smDecls :: [MaybeExported SurfaceDeclaration ann]
    , smTermFixity :: FixityEnv
    , smTypeFixity :: FixityEnv
    }

data MaybeExported thing ann = Exported (thing ann) | Hidden (thing ann)

-- things explicitly not here that might be surprising:
-- SIf - conditionals are done by the `?` glyph, which is `Bool -> a -> a -> a`
-- SLet - the preferred abstraction for binding is the where block
data SurfaceExpr ann
    = SELiteral ann Literal -- 2
    | SEIdentifier (AnnotatedIdent ann) -- x
    | SELambda ann (AnnotatedIdent ann) (SurfaceExpr ann) -- λx → foo
    | SEApp ann (SurfaceExpr ann) (SurfaceExpr ann) -- f x
    | SETypeLambda ann (AnnotatedIdent ann) (SurfaceExpr ann) -- Λx ↦ foo
    | SETypeApp ann (SurfaceExpr ann) (SurfaceType ann) -- f 〈x〉
    | SEWhere ann (SurfaceExpr ann) [SurfaceWhereDeclaration ann] -- expr with a where block
    | SEDo ann [SurfaceDoInstruction ann] (SurfaceExpr ann) -- 🝣 \n foo ≔ bar \n baz ↤ quux \n bep
    | SECase ann (SurfaceExpr ann) [SurfaceBranch ann] -- 🝡x \n <pattern match>
    | SELambdaCase ann [SurfaceBranch ann] -- λ🝡 \n <pattern match>
    | SEList ann [SurfaceExpr ann] -- [foo, bar, baz]
    | SETuple ann [SurfaceExpr ann] -- (foo, bar, baz)
    | SEOpSectionL ann (SurfaceExpr ann) (AnnotatedIdent ann) -- (3+)
    | SEOpSectionR ann (AnnotatedIdent ann) (SurfaceExpr ann) -- (+3)
    | SEInfix ann (SurfaceExpr ann) (AnnotatedIdent ann) (SurfaceExpr ann) -- 2+2
    | SEAnnotation ann (SurfaceExpr ann) (SurfaceType ann) -- inline type annotation
    | SEHole ann

data SurfaceBranch ann = SurfaceBranch ann (SurfacePattern ann) (SurfaceExpr ann)

data SurfacePattern ann = SPVar (AnnotatedIdent ann) | SPCon ann (AnnotatedIdent ann) [SurfacePattern ann] | SPWild ann

data SurfaceType ann
    = STName (AnnotatedIdent ann)
    | STApp ann (SurfaceType ann) (SurfaceType ann)
    | STFun ann (SurfaceType ann) (SurfaceType ann)
    | STConstraint ann (SurfaceClassContext ann)
    | STForall ann [AnnotatedIdent ann] (SurfaceType ann)
    | STExists ann [AnnotatedIdent ann] (SurfaceType ann)

data SurfaceTypeConstructor ann = SurfaceTypeConstructor ann DocComment (AnnotatedIdent ann) [SurfaceType ann]

data SurfaceDoInstruction ann
    = SDIBindName ann (AnnotatedIdent ann) (SurfaceExpr ann) -- x ≔ y
    | SDIExtractMonad ann (AnnotatedIdent ann) (SurfaceExpr ann) -- x ↤ y
    | SDIMonadicStmt ann (SurfaceExpr ann) -- x

-- individual types of declarations
data SurfaceTermDecl ann = SurfaceTermDecl ann DocComment (AnnotatedIdent ann) (SurfaceExpr ann) -- binding, body
data SurfaceTypeDecl ann = SurfaceTypeDecl ann DocComment (AnnotatedIdent ann) (SurfaceType ann) -- binding, type
data SurfaceDataDecl ann = SurfaceDataDecl ann DocComment (AnnotatedIdent ann) [MaybeExported SurfaceTypeConstructor ann] -- type name, type constructors
data SurfaceNewtypeDecl ann = SurfaceNewtypeDecl ann DocComment (AnnotatedIdent ann) (MaybeExported SurfaceTypeConstructor ann) -- type name, single type constructor
data SurfaceTypeAlias ann = SurfaceTypeAlias ann DocComment (AnnotatedIdent ann) [AnnotatedIdent ann] (SurfaceType ann) -- alias name, alias parameters, aliasee
data SurfaceConstraintAlias ann = SurfaceConstraintAlias ann DocComment (SurfaceClassApp ann) (SurfaceClassContext ann) -- alias name, aliased context

-- declaration contexts (allow certain subsets of them)
data SurfaceClassMember ann
    = SCMTerm (SurfaceTermDecl ann)
    | SCMType (SurfaceTypeDecl ann)
    | SCMTypeAlias (SurfaceTypeAlias ann)

data SurfaceWhereDeclaration ann
    = SWDTerm (SurfaceTermDecl ann)
    | SWDType (SurfaceTypeDecl ann)

data SurfaceDeclaration ann
    = SDTerm (SurfaceTermDecl ann)
    | SDData (SurfaceDataDecl ann)
    | SDNewtype (SurfaceNewtypeDecl ann)
    | SDTypeAlias (SurfaceTypeAlias ann)
    | SDConstraintAlias (SurfaceConstraintAlias ann)

-- typeclass definitions
data SurfaceClassApp ann = SurfaceClassApp ann (AnnotatedIdent ann) [SurfaceType ann] -- Foo a b c ...
data SurfaceClassContext ann = SurfaceClassContext ann [SurfaceClassApp ann] -- (Foo a b, Bar c d, Baz e f, ...)
