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
import Debug as Debug
import Editor.Lexer as P
import Parsing.Combinators (choice, optionMaybe, try) as P
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (char, string) as P
import Parsing.String.Basic (letter) as P
import Type.Proxy (Proxy(..))

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
  formatBlock _ = map \a -> "- " <> a
  parseBlock p = (UL /\ _) <$> (P.indentP *> P.string "- " *> P.indented (P.many p))

data Code = Code (Maybe String)

derive instance genericCode :: Generic Code _
instance eqCode :: Eq Code where eq = genericEq
instance showCode :: Show Code where show = genericShow

instance blockTypeCode :: BlockType Code where
  blockKind _ = Text
  formatBlock _ as = ["```"] <> as <> ["```"]
  parseBlock p = do
    let langP =   map (String.fromCharArray <<< NEArray.toUnfoldable)
              <$> P.optionMaybe (P.many1 P.letter)
        openP = P.indentP *> P.string "```" *> langP <* P.nl
        closeP = P.indentP *> P.string "```" *> P.nl'
    lang <- openP
    body <- [] `flip tailRecM` \acc -> do
      P.optionMaybe (P.try closeP) >>= case _ of
        Nothing -> P.indentP *> p <#> \a -> Loop (acc <> [a])
        Just _ -> pure $ Done acc
    pure (Code lang /\ body)

type BasicSyntax = Code || H || HR || UL || P

basicSyntax = Proxy :: Proxy BasicSyntax
