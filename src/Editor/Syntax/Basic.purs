module Editor.Syntax.Basic where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (fromCodePointArray) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Tuple.Nested ((/\))
import Editor.AST
import Editor.AST.Block
import Editor.AST.Inline
import Editor.Parser (failMaybe, indentP, inline, intP, nl', manyBetween) as P
import Editor.Syntax (type (||), Syntax(..))
import Editor.Syntax.Helpers as S
import Parsing.Combinators (choice, optionMaybe) as P
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (anyCodePoint, char, string) as P
import Parsing.String.Basic (letter) as P

-- | Inline elements

data B = B

derive instance genericB :: Generic B _
instance eqB :: Eq B where eq = genericEq
instance showB :: Show B where show = genericShow

instance elementB :: Element InlineKind B where
  kind _ = InlineK
  format _ = S.wrappedInlineF "**" "**"
  parse p = P.choice
    [ S.wrappedInlineP (P.string "__" $> B) (P.string "__") p
    , S.wrappedInlineP (P.string "**" $> B) (P.string "**") p
    ]

data I = I

derive instance genericI :: Generic I _
instance eqI :: Eq I where eq = genericEq
instance showI :: Show I where show = genericShow

instance elementI :: Element InlineKind I where
  kind _ = InlineK
  format _ = S.wrappedInlineF "*" "*"
  parse p = P.choice
    [ S.wrappedInlineP (P.string "_" $> I) (P.string "_") p
    , S.wrappedInlineP (P.string "*" $> I) (P.string "*") p
    ]

data S = S

derive instance genericS :: Generic S _
instance eqS :: Eq S where eq = genericEq
instance showS :: Show S where show = genericShow

instance elementS :: Element InlineKind S where
  kind _ = InlineK
  format _ = S.wrappedInlineF "~~" "~~"
  parse = S.wrappedInlineP (P.string "~~" $> S) (P.string "~~")

data A = A String

instance elementA :: Element InlineKind A where
  kind _ = InlineK
  format (A href) as = ["["] <> as <> ["]", "(", href, ")"]
  parse p = do
    r <- P.manyBetween (P.char '[') (P.char ']') p
    href <- String.fromCodePointArray <$> P.manyBetween (P.char '(') (P.char ')') (P.anyCodePoint)
    pure (A href /\ r)

derive instance genericA :: Generic A _
instance eqA :: Eq A where eq = genericEq
instance showA :: Show A where show = genericShow

type BasicInlineSyntax = A || B || S || I

-- | Block elements

data H = H1 | H2 | H3 | H4 | H5 | H6

hn :: H -> Int
hn H1 = 1
hn H2 = 2
hn H3 = 3
hn H4 = 4
hn H5 = 5
hn H6 = 6

nh :: Int -> Maybe H
nh 1 = Just H1
nh 2 = Just H2
nh 3 = Just H3
nh 4 = Just H4
nh 5 = Just H5
nh 6 = Just H6
nh _ = Nothing

derive instance genericH :: Generic H _
instance eqH :: Eq H where eq = genericEq
instance showH :: Show H where show = genericShow

instance elementH :: Element BlockKind H where
  kind _ = PureK
  format h
    = Array.(:)
    $ String.fromCharArray
    $ Array.replicate (hn h) '#'
  parse p = do
    P.indentP
    (/\) <$> P.failMaybe "Invalid header"
             (nh <<< NEArray.length <$> P.many1 (P.char '#') <* P.char ' ')
         <*> P.inline p

data HR = HR

derive instance genericHR :: Generic HR _
instance eqHR :: Eq HR where eq = genericEq
instance showHR :: Show HR where show = genericShow

instance elementHR :: Element BlockKind HR where
  kind _ = UnitK
  format _ _ = ["***"]
  parse _ = P.indentP *> P.choice [ P.char '*', P.char '-' ] >>= \c -> do
    let p = P.optionMaybe (P.char ' ') *> P.char c
    void $ p *> p *> P.many p *> P.nl'
    pure (HR /\ [])

data P = P

derive instance genericP :: Generic P _
instance eqP :: Eq P where eq = genericEq
instance showP :: Show P where show = genericShow

instance elementP :: Element BlockKind P where
  kind _ = PureK
  format _ = identity
  parse p = (P /\ _) <$> (P.indentP *> P.inline p)

data UL = UL

derive instance genericUL :: Generic UL _
instance eqUL :: Eq UL where eq = genericEq
instance showUL :: Show UL where show = genericShow

instance elementUL :: Element BlockKind UL where
  kind _ = NestedK
  format _ = S.listBlockF "- "
  parse = S.listBlockP (P.string "- " *> pure UL)

data OL = OL Int

derive instance genericOL :: Generic OL _
instance eqOL :: Eq OL where eq = genericEq
instance showOL :: Show OL where show = genericShow

instance elementOL :: Element BlockKind OL where
  kind _ = NestedK
  format (OL n) = S.listBlockF (show n <> ". ")
  parse = S.listBlockP (OL <$> P.intP <* P.string ". ")

data Code = Code (Maybe String)

derive instance genericCode :: Generic Code _
instance eqCode :: Eq Code where eq = genericEq
instance showCode :: Show Code where show = genericShow

instance elementCode :: Element BlockKind Code where
  kind _ = TextK
  format _ = S.fencedBlockF "```" "```"
  parse = S.fencedBlockP (Code <$> openP) closeP
    where langP =   map (String.fromCharArray <<< NEArray.toUnfoldable)
                <$> P.optionMaybe (P.many1 P.letter)
          openP = P.string "```" *> langP
          closeP = P.string "```"

data Blockquote = Blockquote

derive instance genericBlockquote :: Generic Blockquote _
instance eqBlockquote :: Eq Blockquote where eq = genericEq
instance showBlockquote :: Show Blockquote where show = genericShow

instance elementBlockquote :: Element BlockKind Blockquote where
  kind _ = NestedK
  format _ = S.multilineBlockF "> "
  parse p = (Blockquote /\ _) <$> S.multilineBlockP (void $ P.string "> ") p

type BasicBlockSyntax = Blockquote || Code || H || HR || UL || OL || P

basicSyntax = Syntax :: Syntax BasicBlockSyntax BasicInlineSyntax
