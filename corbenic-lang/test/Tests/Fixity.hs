module Tests.Fixity (fixityTests) where

import Data.FileEmbed (embedFileRelative)
import Frontend.Lexer (scanTokens)
import Frontend.Parser.Fixity (collectFixities)
import Syntax.Fixity (Fixity (..), FixityEnv, lookupFixity)
import Syntax.Identifier (Identifier (..))
import Test.Tasty
import Test.Tasty.HUnit

-- | Lex a case file and run collectFixities, returning the term and type envs.
runFixity :: String -> (FixityEnv, FixityEnv)
runFixity src =
    case scanTokens src of
        Left e -> error ("lex failed: " <> show e)
        Right (toks, _) -> collectFixities toks

-- | Assert that a name has a given fixity in an env.
assertFixity :: FixityEnv -> Identifier -> Fixity -> Assertion
assertFixity env name expected =
    lookupFixity name env @?= Just expected

fixityTests :: TestTree
fixityTests =
    testGroup
        "Fixity"
        [ testCase "term operator" $ do
            let (term, _) = runFixity (decodeUtf8 $(embedFileRelative "test/Cases/fixity/term_operator.corb"))
            assertFixity term (IdentRaw "+") (RightAssocBinary 6)
        , testCase "type operator" $ do
            let (_, ty) = runFixity (decodeUtf8 $(embedFileRelative "test/Cases/fixity/type_operator.corb"))
            assertFixity ty (IdentRaw "×") (RightAssocBinary 5)
        , testCase "infix constructor" $ do
            let src = decodeUtf8 $(embedFileRelative "test/Cases/fixity/infix_constructor.corb")
            let (term, _) = runFixity src
            assertFixity term (IdentRaw "⋮") (RightAssocBinary 5)
        , testCase "prefix unary" $ do
            let src = decodeUtf8 $(embedFileRelative "test/Cases/fixity/prefix_unary.corb")
            let (term, _) = runFixity src
            assertFixity term (IdentRaw "!") (PrefixUnary 0)
        , testCase "postfix unary" $ do
            let src = decodeUtf8 $(embedFileRelative "test/Cases/fixity/postfix_unary.corb")
            let (term, _) = runFixity src
            assertFixity term (IdentRaw "?") (PostfixUnary 0)
        ]
