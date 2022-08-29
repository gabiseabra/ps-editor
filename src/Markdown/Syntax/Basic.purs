module Markdown.Syntax.Basic where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Either.Nested (type (\/))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (fromCodePointArray) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Tuple.Nested ((/\))
import Markdown.AST (Inline, Block)
import Markdown.Parser (failMaybe, indent, inline, int, line, manyBetween, nl') as P
import Markdown.Syntax (class Element)
import Markdown.Syntax.Helpers as S
import Parsing.Combinators (choice, optionMaybe) as P
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (anyCodePoint, char, string) as P
import Parsing.String.Basic (letter) as P

-- | Inline elements

data B = B

derive instance genericB :: Generic B _
instance eqB :: Eq B where eq = genericEq
instance showB :: Show B where show = genericShow

instance elementB :: Element Inline B where
  parse p = P.choice
    [ S.wrappedP (P.string "__" $> B) (P.string "__") p
    , S.wrappedP (P.string "**" $> B) (P.string "**") p
    ]

data I = I

derive instance genericI :: Generic I _
instance eqI :: Eq I where eq = genericEq
instance showI :: Show I where show = genericShow

instance elementI :: Element Inline I where
  parse p = P.choice
    [ S.wrappedP (P.string "_" $> I) (P.string "_") p
    , S.wrappedP (P.string "*" $> I) (P.string "*") p
    ]

data S = S

derive instance genericS :: Generic S _
instance eqS :: Eq S where eq = genericEq
instance showS :: Show S where show = genericShow

instance elementS :: Element Inline S where
  parse = S.wrappedP (P.string "~~" $> S) (P.string "~~")

data A = A String

instance elementA :: Element Inline A where
  parse p = do
    r <- P.manyBetween (P.char '[') (P.char ']') p
    href <- String.fromCodePointArray <$> P.manyBetween (P.char '(') (P.char ')') (P.anyCodePoint)
    pure (A href /\ r)

derive instance genericA :: Generic A _
instance eqA :: Eq A where eq = genericEq
instance showA :: Show A where show = genericShow

type BasicInlineSyntax = A \/ B \/ S \/ I

-- | Block elements
--------------------------------------------------------------------------------

data P = P

derive instance genericP :: Generic P _
instance eqP :: Eq P where eq = genericEq
instance showP :: Show P where show = genericShow

instance Element Inline P where
  parse p = (P /\ _) <$> (P.indent *> P.inline p)

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

instance elementH :: Element Inline H where
  parse p = do
    P.indent
    (/\) <$> P.failMaybe "Invalid header"
             (nh <<< NEArray.length <$> P.many1 (P.char '#') <* P.char ' ')
         <*> P.inline p

data HR = HR

derive instance genericHR :: Generic HR _
instance eqHR :: Eq HR where eq = genericEq
instance showHR :: Show HR where show = genericShow

instance elementHR :: Element Inline HR where
  parse _ = P.indent *> P.choice [ P.char '*', P.char '-' ] >>= \c -> do
    let p = P.optionMaybe (P.char ' ') *> P.char c
    void $ p *> p *> P.many p *> P.nl'
    pure (HR /\ [])

data UL = UL

derive instance genericUL :: Generic UL _
instance eqUL :: Eq UL where eq = genericEq
instance showUL :: Show UL where show = genericShow

instance elementUL :: Element Block UL where
  parse = S.listP (P.string "- " *> pure UL)

data OL = OL Int

derive instance genericOL :: Generic OL _
instance eqOL :: Eq OL where eq = genericEq
instance showOL :: Show OL where show = genericShow

instance elementOL :: Element Block OL where
  parse = S.listP (OL <$> P.int <* P.string ". ")

data Code = Code (Maybe String) (Array String)

derive instance genericCode :: Generic Code _
instance eqCode :: Eq Code where eq = genericEq
instance showCode :: Show Code where show = genericShow

instance elementCode :: Element Inline Code where
  parse _ = do
    (lang /\ code) <- S.fencedP openP closeP P.line
    pure (Code lang code /\ [])
    where langP =   map (String.fromCharArray <<< NEArray.toUnfoldable)
                <$> P.optionMaybe (P.many1 P.letter)
          openP = P.string "```" *> langP
          closeP = P.string "```"

data Blockquote = Blockquote

derive instance genericBlockquote :: Generic Blockquote _
instance eqBlockquote :: Eq Blockquote where eq = genericEq
instance showBlockquote :: Show Blockquote where show = genericShow

instance elementBlockquote :: Element Block Blockquote where
  parse p = (Blockquote /\ _) <$> S.multilineP (void $ P.string "> ") p

type BasicBlockSyntax = Blockquote \/ Code \/ H \/ HR \/ UL \/ OL \/ P
