module Syntax.Token where

import Prelude

import Syntax.Fixity
import Syntax.Identifier
import Syntax.Literal

-- | A token emitted by the lexer.
data Token
    = TokLiteral Literal
    | TokIdentifier Identifier
    | TokHole
    | -- grouping
      TokLParen
    | TokRParen
    | TokLBracket
    | TokRBracket
    | TokLBrace
    | TokRBrace
    | TokAngleL
    | TokAngleR
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
    | TokAssociatedType
    | -- typeclasses
      TokTypeclassImplies
    | TokTypeclassIntersect
    | TokTypeclassElement
    | TokTypeclassInstance
    | -- syntactic structure
      TokLambda
    | TokTypeLambda
    | TokMapsTo
    | TokCase
    | TokDo
    | TokDoMonadic
    | TokNoExport
    | -- modules
      TokModule
    | TokImport
    | TokQualify
    | TokReexport
    | TokDot
    | -- EOF
      TokEOF
    deriving (Eq, Ord, Show)

instance Pretty Token where
    prettyPrint (TokLiteral l) = prettyPrint l
    prettyPrint (TokIdentifier i) = prettyPrint i
    prettyPrint (TokFixityDecl f) = prettyPrint f
    prettyPrint TokHole = "_"
    prettyPrint TokLParen = "("
    prettyPrint TokRParen = ")"
    prettyPrint TokLBracket = "["
    prettyPrint TokRBracket = "]"
    prettyPrint TokLBrace = "{"
    prettyPrint TokRBrace = "}"
    prettyPrint TokAngleL = "〈"
    prettyPrint TokAngleR = "〉"
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
    prettyPrint TokAssociatedType = "≋"
    prettyPrint TokTypeclassImplies = "⇒"
    prettyPrint TokTypeclassIntersect = "⋒"
    prettyPrint TokTypeclassElement = "⋹"
    prettyPrint TokTypeclassInstance = "∴"
    prettyPrint TokLambda = "λ"
    prettyPrint TokTypeLambda = "Λ"
    prettyPrint TokMapsTo = "↦"
    prettyPrint TokCase = "🝡"
    prettyPrint TokDo = "🝣"
    prettyPrint TokDoMonadic = "↤"
    prettyPrint TokNoExport = "※"
    prettyPrint TokModule = "▣"
    prettyPrint TokImport = "⇲"
    prettyPrint TokQualify = "⌸"
    prettyPrint TokReexport = "⇱"
    prettyPrint TokDot = "."
    prettyPrint TokEOF = "⌿"
