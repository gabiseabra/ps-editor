module Editor.Syntax.Basic where

import Editor.AST
import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Editor.Lexer as P
import Editor.Syntax as S
import Parsing.Combinators (choice, optionMaybe) as P
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (char, string) as P
import Parsing.String.Basic (letter) as P
import Type.Proxy (Proxy(..))

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

instance blockTypeH :: BlockType H where
  blockKind _ = Pure
  formatBlock h
    = Array.(:)
    $ String.fromCharArray
    $ Array.replicate (hn h) '#'
  parseBlock p = do
    P.indentP
    (/\) <$> P.failMaybe "Invalid header"
             (nh <<< NEArray.length <$> P.many1 (P.char '#') <* P.char ' ')
         <*> map pure p

data HR = HR

derive instance genericHR :: Generic HR _
instance eqHR :: Eq HR where eq = genericEq
instance showHR :: Show HR where show = genericShow

instance blockTypeHR :: BlockType HR where
  blockKind _ = Unit
  formatBlock _ _ = ["***"]
  parseBlock _ = P.indentP *> P.choice [ P.char '*', P.char '-' ] >>= \c -> do
    let p = P.optionMaybe (P.char ' ') *> P.char c
    void $ p *> p *> P.many p *> P.nl'
    pure (HR /\ [])

data P = P

derive instance genericP :: Generic P _
instance eqP :: Eq P where eq = genericEq
instance showP :: Show P where show = genericShow

instance blockTypeP :: BlockType P where
  blockKind _ = Pure
  formatBlock _ = identity
  parseBlock p = (P /\ _) <$> map pure (P.indentP *> p)

data UL = UL

derive instance genericUL :: Generic UL _
instance eqUL :: Eq UL where eq = genericEq
instance showUL :: Show UL where show = genericShow

instance blockTypeUL :: BlockType UL where
  blockKind _ = Block
  formatBlock _ = S.listBlockF "- "
  parseBlock = S.listBlockP (P.string "- " *> pure UL)

data OL = OL Int

derive instance genericOL :: Generic OL _
instance eqOL :: Eq OL where eq = genericEq
instance showOL :: Show OL where show = genericShow

instance blockTypeOL :: BlockType OL where
  blockKind _ = Block
  formatBlock (OL n) = S.listBlockF (show n <> ". ")
  parseBlock = S.listBlockP (OL <$> P.intP <* P.string ". ")

data Code = Code (Maybe String)

derive instance genericCode :: Generic Code _
instance eqCode :: Eq Code where eq = genericEq
instance showCode :: Show Code where show = genericShow

instance blockTypeCode :: BlockType Code where
  blockKind _ = Text
  formatBlock _ = S.fencedBlockF "```" "```"
  parseBlock = S.fencedBlockP (Code <$> openP) closeP
    where langP =   map (String.fromCharArray <<< NEArray.toUnfoldable)
                <$> P.optionMaybe (P.many1 P.letter)
          openP = P.string "```" *> langP
          closeP = P.string "```"

data Blockquote = Blockquote

derive instance genericBlockquote :: Generic Blockquote _
instance eqBlockquote :: Eq Blockquote where eq = genericEq
instance showBlockquote :: Show Blockquote where show = genericShow

instance blockTypeBlockquote :: BlockType Blockquote where
  blockKind _ = Block
  formatBlock _ = S.multilineBlockF "> "
  parseBlock p = (Blockquote /\ _) <$> S.multilineBlockP (void $ P.string "> ") p

-- | Inline elements

data Span = Span String

derive instance genericSpan :: Generic Span _
instance eqSpan :: Eq Span where eq = genericEq
instance showSpan :: Show Span where show = genericShow

data B = B String

derive instance genericB :: Generic B _
instance eqB :: Eq B where eq = genericEq
instance showB :: Show B where show = genericShow

data I = I String

derive instance genericI :: Generic I _
instance eqI :: Eq I where eq = genericEq
instance showI :: Show I where show = genericShow

data S = S String

derive instance genericS :: Generic S _
instance eqS :: Eq S where eq = genericEq
instance showS :: Show S where show = genericShow

data A = A String String

derive instance genericA :: Generic A _
instance eqA :: Eq A where eq = genericEq
instance showA :: Show A where show = genericShow


type BasicSyntax = Blockquote || Code || H || HR || UL || OL || P

basicSyntax = Proxy :: Proxy BasicSyntax
