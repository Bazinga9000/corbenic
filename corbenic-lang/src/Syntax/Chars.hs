-- | The single source of truth for Corbenic's character inventory.
module Syntax.Chars where

import Prelude

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Syntax.Token

-- Reserved characters ------------------------------------------------------

-- characters that map directly to tokens
reservedTokens :: Map Char Token
reservedTokens =
    Map.fromList
        [ ('(', TokLParen)
        , (')', TokRParen)
        , ('[', TokLBracket)
        , (']', TokRBracket)
        , ('{', TokLBrace)
        , ('}', TokRBrace)
        , ('〈', TokAngleL)
        , ('〉', TokAngleR)
        , ('_', TokHole)
        , (',', TokComma)
        , ('≔', TokTermDecl)
        , ('⠛', TokHasType)
        , ('∀', TokForAll)
        , ('∃', TokExists)
        , ('≣', TokConstraintAlias)
        , ('≘', TokTypeAlias)
        , ('≛', TokNewtype)
        , ('≗', TokData)
        , ('¦', TokConstructorBar)
        , ('≋', TokAssociatedType)
        , ('⇒', TokTypeclassImplies)
        , ('⋒', TokTypeclassIntersect)
        , ('⋹', TokTypeclassElement)
        , ('∴', TokTypeclassInstance)
        , ('λ', TokLambda)
        , ('Λ', TokTypeLambda)
        , ('↦', TokMapsTo)
        , ('🝡', TokCase)
        , ('🝣', TokDo)
        , ('↤', TokDoMonadic)
        , ('※', TokNoExport)
        , ('▣', TokModule)
        , ('⇲', TokImport)
        , ('⌸', TokQualify)
        , ('⇱', TokReexport)
        , ('.', TokDot)
        , ('⏎', TokNewline)
        ]

-- handled specifically by the lexer
charComment :: Char
charComment = '⍝'

charQuote :: Char
charQuote = '\''

charTextQuote :: Char
charTextQuote = '"'

charFixityDeclarator :: Char
charFixityDeclarator = '⦿'

charFixityPrefixUnary :: Char
charFixityPrefixUnary = '⟓'

charFixityPostfixUnary :: Char
charFixityPostfixUnary = 'Ŀ'

charCornerL :: Char
charCornerL = '⌞'

charCornerR :: Char
charCornerR = '⌟'

reservedChars :: Set Char
reservedChars =
    Set.fromList $
        Map.keys reservedTokens
            ++ map fst qBracketPairs
            ++ map snd qBracketPairs
            ++ [charComment, charQuote, charTextQuote, charFixityDeclarator, charFixityPrefixUnary, charFixityPostfixUnary]

isReserved :: Char -> Bool
isReserved = (`Set.member` reservedChars)

-- Identifier suffixes ------------------------------------------------------

suffixChars :: Set Char
suffixChars =
    Set.fromList $
        [charCornerL, charCornerR]
            ++ "′″‴‵‶‷"
            ++ "∁"
            ++ "͚"
            ++ "₀₁₂₃₄₅₆₇₈₉"

isSuffixChar :: Char -> Bool
isSuffixChar = (`Set.member` suffixChars)

-- Quoting brackets ---------------------------------------------------------

data QBracket
    = QPrimitive -- ﴾…﴿  primitives
    | QGuillemet -- «…»   general multichar identifiers
    deriving (Eq, Show, Ord, Enum, Bounded)

qBracketPairs :: [(Char, Char)]
qBracketPairs = [(qOpen q, qClose q) | q <- universe]

qOpen :: QBracket -> Char
qOpen QPrimitive = '﴾'
qOpen QGuillemet = '«'

qClose :: QBracket -> Char
qClose QPrimitive = '﴿'
qClose QGuillemet = '»'

bracketName :: QBracket -> Text
bracketName QPrimitive = "ornate"
bracketName QGuillemet = "guillemet"

qBracketOf :: Char -> Maybe QBracket
qBracketOf c = List.lookup c [(qOpen q, q) | q <- universe]

-- Identifier classes -------------------------------------------------------

data IdentifierClass
    = ClassLatin
    | ClassSerif
    | ClassItalic
    | ClassBold
    | ClassScript
    | ClassFraktur
    | ClassDoubleStruck
    | ClassBoldFraktur
    | ClassSans
    | ClassSansBold
    | ClassSansItalic
    | ClassMonospace
    deriving (Eq, Show, Ord, Enum, Bounded)

classLetters :: IdentifierClass -> (String, String)
classLetters = \case
    ClassLatin -> ("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz")
    ClassSerif -> ("𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙", "𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳")
    ClassItalic -> ("𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍", "𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧")
    ClassBold -> ("𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙", "𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳")
    ClassScript -> ("𝒜ℬ𝒞𝒟ℰℱ𝒢ℋℐ𝒥𝒦ℒℳ𝒩𝒪𝒫𝒬ℛ𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵", "𝒶𝒷𝒸𝒹ℯ𝒻ℊ𝒽𝒾𝒿𝓀𝓁𝓂𝓃ℴ𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏")
    ClassFraktur -> ("𝔄𝔅ℭ𝔇𝔈𝔉𝔊ℌℑ𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔ℜ𝔖𝔗𝔘𝔙𝔚𝔛𝔜ℨ", "𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷")
    ClassDoubleStruck -> ("𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ", "𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫")
    ClassBoldFraktur -> ("𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅", "𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟")
    ClassSans -> ("𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹", "𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓")
    ClassSansBold -> ("𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭", "𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇")
    ClassSansItalic -> ("𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡", "𝘢𝘣𝘤𝘥𝘦𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻")
    ClassMonospace -> ("𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉", "𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣")

classTable :: Map Char IdentifierClass
classTable =
    Map.fromList
        [ (c, cls)
        | cls <- universe
        , let (ups, lows) = classLetters cls
        , c <- ups ++ lows
        ]

identifierClass :: Char -> Maybe IdentifierClass
identifierClass = (`Map.lookup` classTable)

isUpperOf :: IdentifierClass -> Char -> Bool
isUpperOf cls c = identifierClass c == Just cls && c `elem` ups
  where
    (ups, _) = classLetters cls

isLowerOf :: IdentifierClass -> Char -> Bool
isLowerOf cls c = identifierClass c == Just cls && not (isUpperOf cls c)

isLetterOf :: IdentifierClass -> Char -> Bool
isLetterOf cls c = identifierClass c == Just cls

-- Digits, whitespace, comments ---------------------------------------------

isSubDigit :: Char -> Bool
isSubDigit c = c >= '₀' && c <= '₉'

isMidlineSpace :: Char -> Bool
isMidlineSpace c = c == ' ' || c == '\t'

isCommentStart :: Char -> Bool
isCommentStart = (== charComment)
