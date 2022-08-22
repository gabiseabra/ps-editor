module Editor.Lexer where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader as Reader
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Debug as Debug
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.Token (digit) as P

type Scope =
  { indent :: Int
  , pos :: P.Position
  }

emptyScope = { indent: 0, pos: P.initialPos } :: Scope

type Parser = P.ParserT String (ReaderT Scope Identity)

indentP :: Parser Unit
indentP = scopeLine >>= case _ of
  0 -> pure unit
  _ -> do
    { indent } <- lift Reader.ask
    void $ P.replicateM indent $ P.string "  "

indented :: forall p. Parser p -> Parser p
indented p = do
  pos <- P.position
  p `flip P.mapParserT` Reader.local \(s :: Scope) ->
    s { indent = s.indent + 1
      , pos = pos
      }

scopeLine :: Parser Int
scopeLine = do
  { pos: P.Position { line } } <- lift Reader.ask
  (P.Position { line: line' }) <- P.position
  pure (line' - line)

nl = void $ P.char '\n' :: Parser Unit
nl' = nl <|> P.eof :: Parser Unit

nextLine :: Parser Unit
nextLine = nl *> indentP

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
