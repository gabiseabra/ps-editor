module Markdown.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader as Reader
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (foldr)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCodePointArray) as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Tuple (fst)
import Parsing (ParserT, Position(..), fail, initialPos, mapParserT, position) as P
import Parsing.Combinators (choice, lookAhead, many1, optionMaybe) as P
import Parsing.Combinators.Array (manyTill_) as P
import Parsing.String (anyCodePoint, char, eof, string) as P
import Parsing.Token (digit) as P

data Scope = Scope
  { indentStack :: Array (Parser Unit)
  , pos :: P.Position
  }

type Parser = P.ParserT String (ReaderT Scope Identity)

emptyScope = Scope
  { indentStack: []
  , pos: P.initialPos
  } :: Scope

getScope = lift Reader.ask :: Parser Scope

getScopeLine :: Parser Int
getScopeLine = do
  Scope { pos: P.Position { line: ln } } <- getScope
  P.Position { line: ln' } <- P.position
  pure (ln' - ln)

nl = void $ P.char '\n' :: Parser Unit
nl' = nl <|> P.eof :: Parser Unit
indentation = void $ P.string "  " :: Parser Unit

-- Consume indentation
indent :: Parser Unit
indent = getScopeLine >>= case _ of
  0 -> pure unit
  _ -> do
    Scope { indentStack } <- getScope
    foldr (\p p' -> p' *> p) (pure unit) indentStack

indented :: forall p. Parser Unit -> Parser p -> Parser p
indented i p = do
  pos <- P.position
  p `flip P.mapParserT` Reader.local \(Scope s) ->
    Scope
      { indentStack: [i] <> s.indentStack
      , pos: pos
      }

indented_ :: forall p. Parser p -> Parser p
indented_ = indented indentation

inline :: forall a. Parser a -> Parser (Array a)
inline p = do
  P.optionMaybe P.eof >>= maybe (pure unit) (const $ P.fail "EOF")
  tailRecM go []
  where go as = P.optionMaybe nl' >>= case _ of
          Nothing -> p <#> \a -> Loop $ as <> [a]
          Just _  -> pure $ Done as

line = String.fromCodePointArray <$> inline P.anyCodePoint :: Parser String

failMaybe :: forall a. String -> Parser (Maybe a) -> Parser a
failMaybe err p = p >>= case _ of 
  Nothing -> P.fail err
  Just a -> pure a

int :: Parser Int
int = failMaybe "Failed to read digit"
        $ Int.fromString <<< String.fromCharArray <<< NEL.toUnfoldable
       <$> P.many1 P.digit

manyBetween :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (Array p)
manyBetween open close p =  open *> manyTill' p close

manyTill' :: forall close p. Parser p -> Parser close -> Parser (Array p)
manyTill' p close = fst <$> P.manyTill_ p close

-- A parser that consumes characters until a space, newline, or eof is reached
-- without consuming the space or newline
word = String.fromCodePointArray
    <$> manyTill'
        P.anyCodePoint
        (P.lookAhead $ P.choice [void $ P.char ' ',  nl']) :: Parser String
