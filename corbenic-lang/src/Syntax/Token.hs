module Syntax.Token where

import Prelude

import Syntax.Fixity
import Syntax.Identifier
import Syntax.Literal

-- | A token emitted by the lexer.
data Token
    = TokLiteral Literal
    | TokIdentifier Identifier
    | -- grouping
      TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokLBrace
    | TokRBrace
    | TokComma
    | -- lines and indentation
      TokNewline
    | TokIndent
    | TokDedent
    | -- documentation
      TokDocComment Text
    | -- term declarations
      TokTermDecl
    | TokHasType
    | TokFixityDecl Fixity
    | -- type declaration tokens
      TokForAll
    | TokExists
    | -- type declarations
      TokConstraintAlias
    | TokTypeAlias
    | TokNewtype
    | TokData
    | TokConstructorBar
    | -- typeclasses
      TokTypeclassImplies
    | TokTypeclassIntersect
    | TokTypeclassElement
    | -- syntactic structure
      TokLambda
    | TokTypeLambda
    | TokMapsTo
    | TokCase
    | TokDo
    | TokDoMonadic
    | TokNoExport
    | -- EOF
      TokEOF
    deriving (Eq, Show)

instance Pretty Token where
    prettyPrint (TokLiteral l) = prettyPrint l
    prettyPrint (TokIdentifier i) = prettyPrint i
    prettyPrint (TokFixityDecl f) = prettyPrint f
    prettyPrint TokLParen = "("
    prettyPrint TokRParen = ")"
    prettyPrint TokLBracket = "["
    prettyPrint TokRBracket = "]"
    prettyPrint TokLBrace = "{"
    prettyPrint TokRBrace = "}"
    prettyPrint TokComma = ","
    prettyPrint TokNewline = "⏎"
    prettyPrint TokIndent = "⇥"
    prettyPrint TokDedent = "⇤"
    prettyPrint (TokDocComment t) = "⍝⍝ " <> t
    prettyPrint TokTermDecl = "≔"
    prettyPrint TokHasType = "⠛"
    prettyPrint TokForAll = "∀"
    prettyPrint TokExists = "∃"
    prettyPrint TokConstraintAlias = "≣"
    prettyPrint TokTypeAlias = "≘"
    prettyPrint TokNewtype = "≛"
    prettyPrint TokData = "≗"
    prettyPrint TokConstructorBar = "¦"
    prettyPrint TokTypeclassImplies = "⇒"
    prettyPrint TokTypeclassIntersect = "⋒"
    prettyPrint TokTypeclassElement = "⋹"
    prettyPrint TokLambda = "λ"
    prettyPrint TokTypeLambda = "Λ"
    prettyPrint TokMapsTo = "↦"
    prettyPrint TokCase = "🝡"
    prettyPrint TokDo = "🝣"
    prettyPrint TokDoMonadic = "↤"
    prettyPrint TokNoExport = "※"
    prettyPrint TokEOF = "⌿"
