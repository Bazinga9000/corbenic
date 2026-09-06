module Tests.Cases where

import Data.FileEmbed (embedFileRelative)
import Frontend.Lexer
import Syntax.Chars
import Test.Tasty
import Test.Tasty.HUnit

data TestOutcome
    = Okay
    | LexE LexErrorKind
    deriving (Eq, Show)

runCase :: TestOutcome -> String -> Assertion
runCase expected src = do
    case scanTokens src of
        Left (LexError _ k) -> expected @?= LexE k
        Right _ -> expected @?= Okay

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
    ]

caseTests :: TestTree
caseTests = testGroup "File Cases" (map mkCase cases)
  where
    mkCase :: (String, String, TestOutcome) -> TestTree
    mkCase (name, src, outcome) = testCase name (runCase outcome src)
