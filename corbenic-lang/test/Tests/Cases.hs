module Tests.Cases where

import Data.FileEmbed (embedFileRelative)
import Frontend.Lexer
import Frontend.Parser (parse)
import Frontend.Parser.Types (ParseError (..), ParseErrorKind (..))
import Syntax.Chars
import Test.Tasty
import Test.Tasty.HUnit

data TestOutcome
    = Okay
    | LexE LexErrorKind
    | ParseE ParseErrorKind
    deriving (Eq, Show)

runCase :: TestOutcome -> String -> Assertion
runCase expected src = do
    outcome <- case scanTokens src of
        Left (LexError _ k) -> pure (LexE k)
        Right (toks, _) -> case parse toks of
            Left (ParseError _ k) -> pure (ParseE k)
            Right _ -> pure Okay
    expected @?= outcome

cases :: [(String, String, TestOutcome)]
cases =
    [ ("bad_fixity", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/bad_fixity.corb"), LexE (LexBadFixity "⦿⌟"))
    , ("indent_jump", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/indent_jump.corb"), LexE (LexLayoutJumped 2))
    , ("odd_indent", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/odd_indent.corb"), LexE (LexInvalidIndentLevel 1))
    , ("suffix_alone", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/suffix_alone.corb"), LexE (LexUnexpected '′'))
    , ("unterminated_char", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_char.corb"), LexE (LexUnexpected '\''))
    , ("unterminated_guillemet", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_guillemet.corb"), LexE (LexUnterminated QGuillemet))
    , ("unterminated_ornate", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_ornate.corb"), LexE (LexUnterminated QPrimitive))
    , ("unterminated_text", decodeUtf8 $(embedFileRelative "test/Cases/lexerr/unterminated_text.corb"), LexE (LexUnexpected '"'))
    , ("missing_module_glyph", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/missing_module_glyph.corb"), ParseE ParseSyntaxError)
    , ("missing_module_name", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/missing_module_name.corb"), ParseE ParseSyntaxError)
    , ("import_missing_path", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/import_missing_path.corb"), ParseE ParseSyntaxError)
    , ("import_block_unclosed", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/import_block_unclosed.corb"), ParseE ParseSyntaxError)
    , ("import_block_no_dedent", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_block_no_dedent.corb"), Okay)
    , ("bare_module", decodeUtf8 $(embedFileRelative "test/Cases/ok/bare_module.corb"), Okay)
    , ("dotted_module", decodeUtf8 $(embedFileRelative "test/Cases/ok/dotted_module.corb"), Okay)
    , ("import_simple", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_simple.corb"), Okay)
    , ("import_reexport", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_reexport.corb"), Okay)
    , ("import_qualified", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_qualified.corb"), Okay)
    , ("import_block", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_block.corb"), Okay)
    , ("import_except", decodeUtf8 $(embedFileRelative "test/Cases/ok/import_except.corb"), Okay)
    , ("term_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/term_decl.corb"), Okay)
    , ("term_decl_annotated", decodeUtf8 $(embedFileRelative "test/Cases/ok/term_decl_annotated.corb"), Okay)
    , ("lambda", decodeUtf8 $(embedFileRelative "test/Cases/ok/lambda.corb"), Okay)
    , ("lambda_case", decodeUtf8 $(embedFileRelative "test/Cases/ok/lambda_case.corb"), Okay)
    , ("case_expr", decodeUtf8 $(embedFileRelative "test/Cases/ok/case_expr.corb"), Okay)
    , ("do_block", decodeUtf8 $(embedFileRelative "test/Cases/ok/do_block.corb"), Okay)
    , ("do_block_mid", decodeUtf8 $(embedFileRelative "test/Cases/ok/do_block_mid.corb"), Okay)
    , ("list", decodeUtf8 $(embedFileRelative "test/Cases/ok/list.corb"), Okay)
    , ("tuple", decodeUtf8 $(embedFileRelative "test/Cases/ok/tuple.corb"), Okay)
    , ("infix", decodeUtf8 $(embedFileRelative "test/Cases/ok/infix.corb"), Okay)
    , ("section_l", decodeUtf8 $(embedFileRelative "test/Cases/ok/section_l.corb"), Okay)
    , ("section_r", decodeUtf8 $(embedFileRelative "test/Cases/ok/section_r.corb"), Okay)
    , ("annotation", decodeUtf8 $(embedFileRelative "test/Cases/ok/annotation.corb"), Okay)
    , ("hole", decodeUtf8 $(embedFileRelative "test/Cases/ok/hole.corb"), Okay)
    , ("where_block", decodeUtf8 $(embedFileRelative "test/Cases/ok/where_block.corb"), Okay)
    , ("data_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/data_decl.corb"), Okay)
    , ("data_decl_block", decodeUtf8 $(embedFileRelative "test/Cases/ok/data_decl_block.corb"), Okay)
    , ("newtype_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/newtype_decl.corb"), Okay)
    , ("type_alias", decodeUtf8 $(embedFileRelative "test/Cases/ok/type_alias.corb"), Okay)
    , ("constraint_alias", decodeUtf8 $(embedFileRelative "test/Cases/ok/constraint_alias.corb"), Okay)
    , ("class_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/class_decl.corb"), Okay)
    , ("class_decl_default", decodeUtf8 $(embedFileRelative "test/Cases/ok/class_decl_default.corb"), Okay)
    , ("class_associated_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/class_associated_type.corb"), Okay)
    , ("instance_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/instance_decl.corb"), Okay)
    , ("forall_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/forall_type.corb"), Okay)
    , ("exists_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/exists_type.corb"), Okay)
    , ("list_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/list_type.corb"), Okay)
    , ("tuple_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/tuple_type.corb"), Okay)
    , ("constraint_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/constraint_type.corb"), Okay)
    , ("type_app", decodeUtf8 $(embedFileRelative "test/Cases/ok/type_app.corb"), Okay)
    , ("fun_type", decodeUtf8 $(embedFileRelative "test/Cases/ok/fun_type.corb"), Okay)
    , ("type_lambda", decodeUtf8 $(embedFileRelative "test/Cases/ok/type_lambda.corb"), Okay)
    , ("type_app_expr", decodeUtf8 $(embedFileRelative "test/Cases/ok/type_app_expr.corb"), Okay)
    , ("type_app_multi", decodeUtf8 $(embedFileRelative "test/Cases/ok/type_app_multi.corb"), Okay)
    , ("prefix_unary", decodeUtf8 $(embedFileRelative "test/Cases/ok/prefix_unary.corb"), Okay)
    , ("class_superclass", decodeUtf8 $(embedFileRelative "test/Cases/ok/class_superclass.corb"), Okay)
    , ("class_set_context", decodeUtf8 $(embedFileRelative "test/Cases/ok/class_set_context.corb"), Okay)
    , ("hidden_decl", decodeUtf8 $(embedFileRelative "test/Cases/ok/hidden_decl.corb"), Okay)
    , ("unclosed_list", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/unclosed_list.corb"), ParseE ParseSyntaxError)
    , ("unclosed_tuple", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/unclosed_tuple.corb"), ParseE ParseSyntaxError)
    , ("do_block_ends_bind", decodeUtf8 $(embedFileRelative "test/Cases/ok/do_block_ends_bind.corb"), Okay)
    , ("term_decl_no_body", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/term_decl_no_body.corb"), ParseE ParseSyntaxError)
    , ("infix_self", decodeUtf8 $(embedFileRelative "test/Cases/parseerr/infix_self.corb"), ParseE ParseSyntaxError)
    ]

caseTests :: TestTree
caseTests = testGroup "File Cases" (map mkCase cases)
  where
    mkCase :: (String, String, TestOutcome) -> TestTree
    mkCase (name, src, outcome) = testCase name (runCase outcome src)
