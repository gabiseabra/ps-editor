module Editor.Lexer where

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
import Data.String as String
import Data.String.CodeUnits (fromCharArray) as String
import Debug as Debug
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.Token (digit) as P

data Scope = Scope
  { indent :: Array (Parser Unit)
  , pos :: P.Position
  }

type Parser = P.ParserT String (ReaderT Scope Identity)

emptyScope = Scope { indent: [], pos: P.initialPos } :: Scope

getScope = lift Reader.ask :: Parser Scope

getScopeLine :: Parser Int
getScopeLine = do
  Scope { pos: P.Position { line } } <- getScope
  P.Position { line: line' } <- P.position
  pure (line' - line)

nl = void $ P.char '\n' :: Parser Unit
nl' = nl <|> P.eof :: Parser Unit
indent = void $ P.string "  " :: Parser Unit

indentP :: Parser Unit
indentP = getScopeLine >>= case _ of
  0 -> pure unit
  _ -> do
    Scope { indent } <- getScope
    foldr (\p p' -> p' *> p) (pure unit) indent

indented :: forall p. Parser Unit -> Parser p -> Parser p
indented i p = do
  pos <- P.position
  p `flip P.mapParserT` Reader.local \(Scope s) ->
    Scope
      { indent: [i] <> s.indent
      , pos: pos
      }

indented_ :: forall p. Parser p -> Parser p
indented_ = indented indent

-- Consume the contents of a single line but not the newline character
-- TODO: keep going if the line ends in 2 spaces or backslash
line :: forall a. Parser a -> Parser String
line p = do
  P.optionMaybe P.eof >>= maybe (pure unit) (const $ P.fail "EOF")
  P.ParseState input1 _ _ <- P.getParserT
  input2 <- tailRecM go unit
  pure $ String.take (String.length input1 - String.length input2) input1
  where
    go unit =
      P.optionMaybe nl' >>= case _ of
        Nothing -> void p $> Loop unit
        Just _  -> P.getParserT <#> \(P.ParseState i2 _ _) -> Done i2

line_ :: Parser String
line_ = line P.anyCodePoint

failMaybe :: forall a. String -> Parser (Maybe a) -> Parser a
failMaybe err p = p >>= case _ of 
  Nothing -> P.fail err
  Just a -> pure a

intP :: Parser Int
intP = failMaybe "Failed to read digit"
        $ Int.fromString <<< String.fromCharArray <<< NEL.toUnfoldable
       <$> P.many1 P.digit
