module Tests.Lexer (lexerTests) where

import Frontend.Lexer (LexError (..), LexErrorKind (..), LexWarning (..), LexWarningKind (..), scanTokens)
import Syntax.Chars (QBracket (..))
import Syntax.Fixity
import Syntax.Identifier
import Syntax.Literal
import Syntax.Location
import Syntax.Token
import Prelude

import Data.Ratio ((%))
import Test.Tasty
import Test.Tasty.HUnit

-- lex and ignore warnings
toks :: String -> Either LexError [Token]
toks s = map (\(Annotated _ a) -> a) . fst <$> scanTokens s

-- assert that lexing fails with a certain error
failsWith :: String -> LexErrorKind -> Assertion
failsWith s p = case scanTokens s of
    Left (LexError _ k) -> if k == p then pass else assertFailure ("wrong lex error kind: " <> show k)
    other -> assertFailure ("expected a lex error, got " <> show other)

lexerTests :: TestTree
lexerTests =
    testGroup
        "Lexer"
        [ testGroup
            "Literals"
            [ testCase "natural" $
                toks "42" @?= Right [TokLiteral (LitNatural 42), TokNewline, TokEOF]
            , testCase "rational" $
                toks "1.5" @?= Right [TokLiteral (LitRational (3 % 2)), TokNewline, TokEOF]
            , testCase "decimal dot is part of the literal" $
                toks "1.5" @?= Right [TokLiteral (LitRational (3 % 2)), TokNewline, TokEOF]
            , testCase "dot with spaces is not a decimal" $
                toks "1 . 5" @?= Right [TokLiteral (LitNatural 1), TokDot, TokLiteral (LitNatural 5), TokNewline, TokEOF]
            , testCase "lone zero" $
                toks "0" @?= Right [TokLiteral (LitNatural 0), TokNewline, TokEOF]
            , testCase "bools" $
                toks "True False" @?= Right [TokLiteral (LitBool True), TokLiteral (LitBool False), TokNewline, TokEOF]
            , testCase "char" $
                toks "'x'" @?= Right [TokLiteral (LitChar 'x'), TokNewline, TokEOF]
            , testCase "text" $
                toks "\"hello\"" @?= Right [TokLiteral (LitText "hello"), TokNewline, TokEOF]
            ]
        , testGroup
            "Identifiers"
            [ testCase "single glyphs" $
                toks "+ π ꙮ" @?= Right [ident "+", ident "π", ident "ꙮ", TokNewline, TokEOF]
            , testCase "adjacent glyphs are separate identifiers" $
                toks "f+" @?= Right [ident "f", ident "+", TokNewline, TokEOF]
            , testCase "capital run" $
                toks "Monoid HasComplexUnit" @?= Right [ident "Monoid", ident "HasComplexUnit", TokNewline, TokEOF]
            , testCase "space terminates a capital run" $
                toks "Additive X" @?= Right [ident "Additive", ident "X", TokNewline, TokEOF]
            , testCase "suffixes bind to the identifier" $
                toks "x₂ x′ x⌟ π₂" @?= Right [ident "x₂", ident "x′", ident "x⌟", ident "π₂", TokNewline, TokEOF]
            , testCase "suffix cannot stand alone" $
                failsWith "′" (LexUnexpected '′')
            , testCase "math-alphanumeric class run" $
                toks "𝕆𝕟" @?= Right [ident "𝕆𝕟", TokNewline, TokEOF]
            , testCase "mixed classes split" $
                toks "𝕆𝕟𝖃𝖞𝖟" @?= Right [ident "𝕆𝕟", ident "𝖃𝖞𝖟", TokNewline, TokEOF]
            , testCase "double-struck letterlike ℕ is a class capital" $
                toks "ℕ" @?= Right [ident "ℕ", TokNewline, TokEOF]
            ]
        , testGroup
            "Q-brackets"
            [ testCase "guillemets" $
                toks "«+++»" @?= Right [TokIdentifier (IdentQuoted "+++"), TokNewline, TokEOF]
            , testCase "ornate brackets (primitives)" $
                toks "﴾+ℤℤ﴿" @?= Right [TokIdentifier (IdentPrimitive "+ℤℤ"), TokNewline, TokEOF]
            , testCase "angle brackets are reserved tokens" $
                toks "〈ℕ〉" @?= Right [TokAngleL, TokIdentifier (IdentRaw "ℕ"), TokAngleR, TokNewline, TokEOF]
            , testCase "unterminated guillemet" $
                failsWith "«oops" (LexUnterminated QGuillemet)
            ]
        , testGroup
            "Reserved & declarations"
            [ testCase "grouping" $
                toks "( ) [ ] { } ," @?= Right [TokLParen, TokRParen, TokLBracket, TokRBracket, TokLBrace, TokRBrace, TokComma, TokNewline, TokEOF]
            , testCase "declaration glyphs" $
                toks "≔ ⠛ ≣ ≘ ≛ ≗ ¦ ≋" @?= Right [TokTermDecl, TokHasType, TokConstraintAlias, TokTypeAlias, TokNewtype, TokData, TokConstructorBar, TokAssociatedType, TokNewline, TokEOF]
            , testCase "typeclass operators" $
                toks "⇒ ⋒ ⋹ ∴" @?= Right [TokTypeclassImplies, TokTypeclassIntersect, TokTypeclassElement, TokTypeclassInstance, TokNewline, TokEOF]
            , testCase "module glyphs" $
                toks "▣ ⇲ ⌸ ⇱ ※" @?= Right [TokModule, TokImport, TokQualify, TokReexport, TokNoExport, TokNewline, TokEOF]
            , testCase "dot" $
                toks "." @?= Right [TokDot, TokNewline, TokEOF]
            ]
        , testGroup
            "Fixity"
            [ testCase "right-assoc prec 6" $
                toks "⦿⌟₆" @?= Right [TokFixityDecl (RightAssocBinary 6), TokNewline, TokEOF]
            , testCase "left-assoc prec 6" $
                toks "⦿⌞₆" @?= Right [TokFixityDecl (LeftAssocBinary 6), TokNewline, TokEOF]
            , testCase "non-assoc prec 4" $
                toks "⦿₄" @?= Right [TokFixityDecl (NonAssocBinary 4), TokNewline, TokEOF]
            , testCase "prefix unary" $
                toks "⦿⟓₀" @?= Right [TokFixityDecl (PrefixUnary 0), TokNewline, TokEOF]
            , testCase "postfix unary" $
                toks "⦿Ŀ₀" @?= Right [TokFixityDecl (PostfixUnary 0), TokNewline, TokEOF]
            , testCase "multi-digit precedence" $
                toks "⦿⌟₆₄" @?= Right [TokFixityDecl (RightAssocBinary 64), TokNewline, TokEOF]
            , testCase "lone subscript zero precedence" $
                toks "⦿⌞₀" @?= Right [TokFixityDecl (LeftAssocBinary 0), TokNewline, TokEOF]
            , testCase "fixity without precedence digits" $
                failsWith "⦿⌟" (LexBadFixity "⦿⌟")
            ]
        , testGroup
            "Layout"
            [ testCase "the spec's Additive/Multiplicative example" $
                toks (unlinesS ["⇒ Additive x", "  ⦿⌟₆ + ⠛ x → x → x", "  ⓪ ⠛ x", "", "⇒ Multiplicative x", "  ⦿⌟₅ * ⠛ x → x → x", "  ① ⠛ x"])
                    @?= Right
                        [ TokTypeclassImplies
                        , ident "Additive"
                        , ident "x"
                        , TokNewline
                        , TokIndent
                        , TokFixityDecl (RightAssocBinary 6)
                        , ident "+"
                        , TokHasType
                        , ident "x"
                        , ident "→"
                        , ident "x"
                        , ident "→"
                        , ident "x"
                        , TokNewline
                        , ident "⓪"
                        , TokHasType
                        , ident "x"
                        , TokNewline
                        , TokDedent
                        , TokTypeclassImplies
                        , ident "Multiplicative"
                        , ident "x"
                        , TokNewline
                        , TokIndent
                        , TokFixityDecl (RightAssocBinary 5)
                        , ident "*"
                        , TokHasType
                        , ident "x"
                        , ident "→"
                        , ident "x"
                        , ident "→"
                        , ident "x"
                        , TokNewline
                        , ident "①"
                        , TokHasType
                        , ident "x"
                        , TokNewline
                        , TokDedent
                        , TokEOF
                        ]
            , testCase "blank lines are transparent" $
                toks "a\n\n  b" @?= Right [ident "a", TokNewline, TokIndent, ident "b", TokNewline, TokDedent, TokEOF]
            , testCase "comments are transparent" $
                toks "a ⍝ note\nb" @?= Right [ident "a", TokNewline, ident "b", TokNewline, TokEOF]
            , testCase "doc comments emit a token" $
                toks "⍝⍝ docs for a\na" @?= Right [TokDocComment "docs for a", TokNewline, ident "a", TokNewline, TokEOF]
            , testCase "doc comments are transparent for layout" $
                toks "a\n  ⍝⍝ indented doc\n  b" @?= Right [ident "a", TokNewline, TokDocComment "indented doc", TokNewline, TokIndent, ident "b", TokNewline, TokDedent, TokEOF]
            , testCase "mid-line doc comment is a warning" $
                case scanTokens "a ⍝⍝ not a doc\nb" of
                    Right (_, [LexWarning _ WarnMidlineDocComment]) -> pass
                    other -> assertFailure ("expected a mid-line doc warning, got " <> show other)
            , testCase "odd indentation is a layout error" $
                failsWith "a\n b" (LexInvalidIndentLevel 1)
            , testCase "indent jump of more than one level is an error" $
                failsWith "a\n    b" (LexLayoutJumped 2)
            , testCase "dedent closes blocks" $
                toks "a\n  b\n  c\nd"
                    @?= Right
                        [ ident "a"
                        , TokNewline
                        , TokIndent
                        , ident "b"
                        , TokNewline
                        , ident "c"
                        , TokNewline
                        , TokDedent
                        , ident "d"
                        , TokNewline
                        , TokEOF
                        ]
            ]
        , testGroup
            "Whitespace significance"
            [ testCase "mid-line spaces are insignificant" $
                toks "x ⋹ Monoid" @?= Right [ident "x", TokTypeclassElement, ident "Monoid", TokNewline, TokEOF]
            , testCase "spaces can split adjacent glyphs" $
                toks "a + - b" @?= Right [ident "a", ident "+", ident "-", ident "b", TokNewline, TokEOF]
            ]
        ]
  where
    ident = TokIdentifier . IdentRaw
    unlinesS = concatMap (<> "\n")
