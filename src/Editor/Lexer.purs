module Editor.Lexer where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Identity (Identity)
import Data.Int as Int
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray) as String
import Data.Tuple (fst)
import Debug as Debug
import Parsing as P
import Parsing.Combinators as P
import Parsing.String as P
import Parsing.Token (digit) as P

type ScopeCtx =
  { indent :: Array Int
  }

type Parser = P.ParserT String (Identity)

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
        Nothing -> void p $> Loop unit
        Just _  -> P.getParserT <#> \(P.ParseState i2 _ _) -> Done i2

line_ :: Parser String
line_ = line P.anyCodePoint

multiLine :: forall a b. Parser a -> Parser b -> Parser String
multiLine prefix p = "" `flip tailRecM` \acc -> do
  void $ prefix
  ln <- line p
  let acc' = acc <> ln
  P.optionMaybe (P.lookAhead (nl *> void prefix)) >>= case _ of
    Nothing -> pure $ Done acc'
    Just _  -> nl $> Loop (acc' <> "\n")

multiLine_ :: forall a. Parser a -> Parser String
multiLine_ a = multiLine a P.anyCodePoint

failMaybe :: forall a. String -> Parser (Maybe a) -> Parser a
failMaybe err p = p >>= case _ of 
  Nothing -> P.fail err
  Just a -> pure a

intP :: Parser Int
intP = failMaybe "Failed to read digit"
        $ Int.fromString <<< String.fromCharArray <<< NEL.toUnfoldable
       <$> P.many1 P.digit