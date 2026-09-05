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
    { smName :: Annotated ann SurfaceName
    , smImports :: [SurfaceImport ann]
    , smDecls :: [MaybeExported SurfaceDeclaration ann]
    , smTermFixity :: FixityEnv
    , smTypeFixity :: FixityEnv
    }

data MaybeExported thing ann = Exported (thing ann) | Hidden (thing ann)

type ModulePath = [SurfaceName]

data SurfaceImport ann
    = SurfaceImport ann ModulePath (Maybe Qualifier) (ImportSpec ann) -- ⇲ Foo.Bar [⌸ T] [block]
    | SurfaceReexport ann ModulePath (ImportSpec ann) -- ⇱ Foo.Bar [block]

newtype Qualifier = Qualifier SurfaceName

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

data SurfaceBranch ann = SurfaceBranch ann (SurfacePattern ann) (SurfaceExpr ann)

data SurfacePattern ann = SPVar (Annotated ann SurfaceName) | SPCon ann (Annotated ann SurfaceName) [SurfacePattern ann] | SPWild ann

data SurfaceType ann
    = STName (Annotated ann SurfaceName)
    | STApp ann (SurfaceType ann) (SurfaceType ann)
    | STFun ann (SurfaceType ann) (SurfaceType ann)
    | STConstraint ann (SurfaceClassContext ann)
    | STForall ann [Annotated ann SurfaceName] (SurfaceType ann)
    | STExists ann [Annotated ann SurfaceName] (SurfaceType ann)

data SurfaceTypeConstructor ann = SurfaceTypeConstructor ann DocComment (Annotated ann SurfaceName) [SurfaceType ann]

data SurfaceDoInstruction ann
    = SDIBindName ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- x ≔ y
    | SDIExtractMonad ann (Annotated ann SurfaceName) (SurfaceExpr ann) -- x ↤ y
    | SDIMonadicStmt ann (SurfaceExpr ann) -- x

-- individual types of declarations
data SurfaceTermDecl ann = SurfaceTermDecl ann DocComment (Annotated ann SurfaceDeclName) (SurfaceExpr ann) -- binding, body
data SurfaceTypeDecl ann = SurfaceTypeDecl ann DocComment (Annotated ann SurfaceDeclName) (SurfaceType ann) -- binding, type
data SurfaceDataDecl ann = SurfaceDataDecl ann DocComment (Annotated ann SurfaceDeclName) [MaybeExported SurfaceTypeConstructor ann] -- type name, type constructors
data SurfaceNewtypeDecl ann = SurfaceNewtypeDecl ann DocComment (Annotated ann SurfaceDeclName) (MaybeExported SurfaceTypeConstructor ann) -- type name, single type constructor
data SurfaceTypeAlias ann = SurfaceTypeAlias ann DocComment (Annotated ann SurfaceDeclName) [Annotated ann SurfaceName] (SurfaceType ann) -- alias name, alias parameters, aliasee
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
    = SDTerm (Maybe (SurfaceTypeDecl ann)) (SurfaceTermDecl ann)
    | SDData (SurfaceDataDecl ann)
    | SDNewtype (SurfaceNewtypeDecl ann)
    | SDTypeAlias (SurfaceTypeAlias ann)
    | SDConstraintAlias (SurfaceConstraintAlias ann)

-- typeclass definitions
data SurfaceClassApp ann = SurfaceClassApp ann (Annotated ann SurfaceName) [SurfaceType ann] -- Foo a b c ...
data SurfaceClassContext ann = SurfaceClassContext ann [SurfaceClassApp ann] -- (Foo a b, Bar c d, Baz e f, ...)
