module Tests.Cases where

import Data.FileEmbed (embedFileRelative)
import Frontend.Flags
import Frontend.Lexer
import Frontend.Parser (parse)
import Frontend.Parser.Types (ParseError (..), ParseErrorKind (..))
import Frontend.TypeChecker
import Frontend.TypeChecker.Error (TypeCheckerError (..), TypeCheckerErrorKind (..))
import Syntax.Chars
import Syntax.Identifier (Identifier (..))
import Test.Tasty
import Test.Tasty.HUnit

data TestOutcome
    = LexE LexErrorKind
    | ParseE ParseErrorKind
    | Parses
    | TypeCheckE TcErrorShape
    | TypeChecks
    deriving (Eq, Show)

-- typechecker errors carry data that's annoying to replicate in a test
-- so we only match on the rough shape of the error (the things that are easy to gin up)
data TcErrorShape
    = TcUnboundIdentifier Identifier
    | TcUnboundTypeConstructor Identifier
    | TcUnboundClass Identifier
    | TcCouldNotUnify
    | TcInfiniteType
    | TcInfiniteKind
    | TcIllegalTypeApp
    | TcKindMismatch
    | TcMissingInstance
    | TcAmbiguousType
    | TcAmbiguousTypeVar
    | TcDuplicateDeclaration Identifier
    | TcSkolemEscape
    | TcPatternArity Identifier Natural Natural
    | TcBadDoBlockEnding
    | TcBug Text
    | TcNYI Text
    deriving (Eq, Show)

tcErrorShape :: TypeCheckerErrorKind -> TcErrorShape
tcErrorShape (TCUnboundIdentifier i) = TcUnboundIdentifier i
tcErrorShape (TCUnboundTypeConstructor i) = TcUnboundTypeConstructor i
tcErrorShape (TCUnboundClass i) = TcUnboundClass i
tcErrorShape (TCCouldNotUnify _ _) = TcCouldNotUnify
tcErrorShape (TCInfiniteType _ _) = TcInfiniteType
tcErrorShape (TCInfiniteKind _ _) = TcInfiniteKind
tcErrorShape (TCIllegalTypeApp _) = TcIllegalTypeApp
tcErrorShape (TCKindMismatch _ _) = TcKindMismatch
tcErrorShape (TCMissingInstance _) = TcMissingInstance
tcErrorShape (TCAmbiguousType _) = TcAmbiguousType
tcErrorShape (TCAmbiguousTypeVar _ _) = TcAmbiguousTypeVar
tcErrorShape (TCDuplicateDeclaration i) = TcDuplicateDeclaration i
tcErrorShape (TCSkolemEscape _) = TcSkolemEscape
tcErrorShape (TCPatternArity i n m) = TcPatternArity i n m
tcErrorShape TCBadDoBlockEnding = TcBadDoBlockEnding
tcErrorShape (TCBug t) = TcBug t
tcErrorShape (TCNYI t) = TcNYI t

testFlags :: FrontendFlags
testFlags =
    FrontendFlags
        { _fNoImplicitPrelude = True
        , _fExperimental = False
        , _fWarnError = False
        , _fAllowOrphans = False
        }

-- run the whole frontend, reporting where it stopped
runFrontend :: String -> TestOutcome
runFrontend src = case scanTokens src of
    Left (LexError _ k) -> LexE k
    Right (toks, _) -> case parse testFlags toks of
        Left (ParseError _ k) -> ParseE k
        Right (modl, _) -> case typeCheckModule testFlags modl of
            Left (TypeCheckerError _ k) -> TypeCheckE (tcErrorShape k)
            Right _ -> TypeChecks

-- does the actual outcome satisfy the expectation?
satisfies :: TestOutcome -> TestOutcome -> Bool
satisfies actual expected = case expected of
    LexE k -> actual == LexE k
    ParseE k -> actual == ParseE k
    TypeCheckE k -> actual == TypeCheckE k
    Parses -> case actual of
        LexE _ -> False
        ParseE _ -> False
        _ -> True
    TypeChecks -> actual == TypeChecks

runCase :: TestOutcome -> String -> Assertion
runCase expected src =
    let actual = runFrontend src
     in if satisfies actual expected
            then pass
            else assertFailure $ "expected " <> show expected <> " but got " <> show actual

-- ---------------------------------------------------------------------------
-- file case groups
-- ---------------------------------------------------------------------------

-- cases that should fail in the lexer
lexErrTests :: TestTree
lexErrTests =
    testGroup
        "Lex Errors"
        [ testCase "bad_fixity" $ runCase (LexE (LexBadFixity "⦿⌟")) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/bad_fixity.corb"))
        , testCase "indent_jump" $ runCase (LexE (LexLayoutJumped 2)) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/indent_jump.corb"))
        , testCase "odd_indent" $ runCase (LexE (LexInvalidIndentLevel 1)) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/odd_indent.corb"))
        , testCase "suffix_alone" $ runCase (LexE (LexUnexpected '′')) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/suffix_alone.corb"))
        , testCase "unterminated_char" $ runCase (LexE (LexUnexpected '\'')) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_char.corb"))
        , testCase "unterminated_guillemet" $ runCase (LexE (LexUnterminated QGuillemet)) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_guillemet.corb"))
        , testCase "unterminated_ornate" $ runCase (LexE (LexUnterminated QPrimitive)) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_ornate.corb"))
        , testCase "unterminated_text" $ runCase (LexE (LexUnexpected '"')) (decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_text.corb"))
        ]

-- cases that should fail in the parser
parseErrTests :: TestTree
parseErrTests =
    testGroup
        "Parse Errors"
        [ testCase "missing_module_glyph" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/missing_module_glyph.corb"))
        , testCase "missing_module_name" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/missing_module_name.corb"))
        , testCase "import_missing_path" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/import_missing_path.corb"))
        , testCase "import_block_unclosed" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/import_block_unclosed.corb"))
        , testCase "unclosed_list" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/unclosed_list.corb"))
        , testCase "unclosed_tuple" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/unclosed_tuple.corb"))
        , testCase "term_decl_no_body" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/term_decl_no_body.corb"))
        , testCase "infix_self" $ runCase (ParseE ParseSyntaxError) (decodeUtf8 $(embedFileRelative "test/Cases/parseerr/infix_self.corb"))
        ]

-- cases that should at least lex and parse
parseTests :: TestTree
parseTests =
    testGroup
        "Parses"
        [ testCase "import_block_no_dedent" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_block_no_dedent.corb"))
        , testCase "bare_module" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/bare_module.corb"))
        , testCase "dotted_module" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/dotted_module.corb"))
        , testCase "import_simple" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_simple.corb"))
        , testCase "import_reexport" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_reexport.corb"))
        , testCase "import_qualified" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_qualified.corb"))
        , testCase "import_block" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_block.corb"))
        , testCase "import_except" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/import_except.corb"))
        , testCase "term_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/term_decl.corb"))
        , testCase "term_decl_annotated" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/term_decl_annotated.corb"))
        , testCase "lambda" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/lambda.corb"))
        , testCase "lambda_case" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/lambda_case.corb"))
        , testCase "case_expr" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/case_expr.corb"))
        , testCase "do_block" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/do_block.corb"))
        , testCase "do_block_mid" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/do_block_mid.corb"))
        , testCase "do_block_ends_bind" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/do_block_ends_bind.corb"))
        , testCase "list" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/list.corb"))
        , testCase "tuple" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/tuple.corb"))
        , testCase "infix" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/infix.corb"))
        , testCase "section_l" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/section_l.corb"))
        , testCase "section_r" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/section_r.corb"))
        , testCase "annotation" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/annotation.corb"))
        , testCase "hole" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/hole.corb"))
        , testCase "where_block" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/where_block.corb"))
        , testCase "data_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/data_decl.corb"))
        , testCase "data_decl_block" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/data_decl_block.corb"))
        , testCase "newtype_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/newtype_decl.corb"))
        , testCase "type_alias" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/type_alias.corb"))
        , testCase "constraint_alias" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/constraint_alias.corb"))
        , testCase "class_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/class_decl.corb"))
        , testCase "class_decl_default" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/class_decl_default.corb"))
        , testCase "class_associated_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/class_associated_type.corb"))
        , testCase "instance_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/instance_decl.corb"))
        , testCase "forall_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/forall_type.corb"))
        , testCase "exists_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/exists_type.corb"))
        , testCase "list_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/list_type.corb"))
        , testCase "tuple_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/tuple_type.corb"))
        , testCase "constraint_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/constraint_type.corb"))
        , testCase "type_app" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/type_app.corb"))
        , testCase "fun_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/fun_type.corb"))
        , testCase "type_lambda" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/type_lambda.corb"))
        , testCase "type_app_expr" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/type_app_expr.corb"))
        , testCase "type_app_multi" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/type_app_multi.corb"))
        , testCase "prefix_unary" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/prefix_unary.corb"))
        , testCase "postfix_unary" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/postfix_unary.corb"))
        , testCase "unary_precedence" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/unary_precedence.corb"))
        , testCase "class_superclass" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/class_superclass.corb"))
        , testCase "class_set_context" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/class_set_context.corb"))
        , testCase "hidden_decl" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/hidden_decl.corb"))
        , testCase "parenthesized_type" $ runCase Parses (decodeUtf8 $(embedFileRelative "test/Cases/parses/parenthesized_type.corb"))
        ]

-- cases that should fail in the typechecker
typeCheckErrTests :: TestTree
typeCheckErrTests =
    testGroup
        "Type Check Errors"
        [ testCase "unbound_identifier" $ runCase (TypeCheckE (TcUnboundIdentifier (IdentRaw "x"))) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/unbound_identifier.corb"))
        , testCase "duplicate_declaration" $ runCase (TypeCheckE (TcDuplicateDeclaration (IdentRaw "f"))) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/duplicate_declaration.corb"))
        , testCase "occurs_check" $ runCase (TypeCheckE TcInfiniteType) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/occurs_check.corb"))
        , testCase "unify_mismatch" $ runCase (TypeCheckE TcCouldNotUnify) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/unify_mismatch.corb"))
        , testCase "annotation_mismatch" $ runCase (TypeCheckE TcCouldNotUnify) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/annotation_mismatch.corb"))
        , testCase "type_app_monomorphic" $ runCase (TypeCheckE TcIllegalTypeApp) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/type_app_monomorphic.corb"))
        , testCase "unbound_type_constructor" $ runCase (TypeCheckE (TcUnboundTypeConstructor (IdentRaw "Nope"))) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/unbound_type_constructor.corb"))
        , testCase "unbound_class" $ runCase (TypeCheckE (TcUnboundClass (IdentRaw "NotAClass"))) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/unbound_class.corb"))
        , testCase "kind_mismatch" $ runCase (TypeCheckE TcKindMismatch) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/kind_mismatch.corb"))
        , testCase "infinite_kind" $ runCase (TypeCheckE TcInfiniteKind) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/infinite_kind.corb"))
        , testCase "type_app_kind_mismatch" $ runCase (TypeCheckE TcKindMismatch) (decodeUtf8 $(embedFileRelative "test/Cases/typecheckerr/type_app_kind_mismatch.corb"))
        ]

-- cases that should typecheck (with no implicit prelude)
typeCheckTests :: TestTree
typeCheckTests =
    testGroup
        "Type Checks"
        [ testCase "identity" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/identity.corb"))
        , testCase "const" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/const.corb"))
        , testCase "apply" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/apply.corb"))
        , testCase "where" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/where.corb"))
        , testCase "hole" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/hole.corb"))
        , testCase "list" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/list.corb"))
        , testCase "tuple" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/tuple.corb"))
        , testCase "literals" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/literals.corb"))
        , testCase "annotated_lambda" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/annotated_lambda.corb"))
        , testCase "signature" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/signature.corb"))
        , testCase "forall_signature" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/forall_signature.corb"))
        , testCase "annotated_literal" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/annotated_literal.corb"))
        , testCase "lambda_case" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/lambda_case.corb"))
        , testCase "case_literals" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/case_literals.corb"))
        , testCase "sections" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/sections.corb"))
        , testCase "infix" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/infix.corb"))
        , testCase "type_lambda" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_lambda.corb"))
        , testCase "type_lambda_app" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_lambda_app.corb"))
        , testCase "mutual_recursion" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/mutual_recursion.corb"))
        , testCase "where_generalize" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/where_generalize.corb"))
        , testCase "mixed_signatures" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/mixed_signatures.corb"))
        , testCase "type_app_value" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_app_value.corb"))
        , testCase "type_app_multi" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_app_multi.corb"))
        , testCase "type_app_apply" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_app_apply.corb"))
        , testCase "type_lambda_apply" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/type_lambda_apply.corb"))
        , testCase "higher_kinded_signature" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/higher_kinded_signature.corb"))
        , testCase "higher_kinded_signature_multi" $ runCase TypeChecks (decodeUtf8 $(embedFileRelative "test/Cases/typechecks/higher_kinded_signature_multi.corb"))
        ]

caseTests :: TestTree
caseTests =
    testGroup
        "File Cases"
        [ lexErrTests
        , parseErrTests
        , parseTests
        , typeCheckErrTests
        , typeCheckTests
        ]
