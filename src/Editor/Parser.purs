module Editor.Parser where

import Editor.Types
import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Int as Int
import Data.List (List)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple, fst)
import Debug as Debug
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.Token (digit) as P
import Partial.Unsafe (unsafeCrashWith)

type Parser = P.Parser String

nl = void $ P.char '\n'
nl' = nl <|> P.eof

anyBetween :: forall a. Parser a -> Parser a -> Parser String
anyBetween open close = P.tryRethrow $ (void open) *> (fst <$> P.anyTill (void close))

blockBetween :: forall a. Parser a -> Parser String
blockBetween p = anyBetween (void p <* nl) (nl *> void p)

line :: forall a. Parser a -> Parser String
line p = do
  P.ParseState input1 _ _ <- P.getParserT
  input2 <- tailRecM go unit
  pure $ String.take (String.length input1 - String.length input2) input1
  where
    go unit =
      P.optionMaybe (P.lookAhead nl') >>= case _ of
        Nothing -> do
          void p
          pure $ Loop unit
        Just _  -> do
          P.ParseState input2 _ _ <- P.getParserT
          pure $ Done input2

failMaybe :: forall a. String -> Parser (Maybe a) -> Parser a
failMaybe err p = p >>= case _ of 
  Nothing -> P.fail err
  Just a -> pure a

intP :: Parser Int
intP = failMaybe "Failed to read digit"
        $ Int.fromString <<< String.fromCharArray <<< NEL.toUnfoldable
       <$> P.many1 P.digit

indentP :: Parser Indent
indentP = P.choice
  [ IN <$ P.string "  "
  , UL <$ P.string "- "
  , OL <$> intP <* P.string ". "
  ]

blockP :: Parser (Block String)
blockP = P.choice
  [ Code       <$> blockBetween (P.string "```")
  , BlockQuote <$> (P.string "> " *> line P.anyCodePoint)
  , Indented <$> indentP <*> (P <$> (P.string "- " *> line P.anyCodePoint))
  , HR         <$  P.replicateM 3 (P.char '-') <* line (P.char '-')
  , P          <$> line P.anyCodePoint
  ] <* nl'

markdownP :: Parser (List (Block String))
markdownP = P.manyTill blockP P.eof
