module Markdown.Syntax.Helpers where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Markdown.Parser (Parser)
import Markdown.Parser (indentation, indent, indented, indented_, nl, nl') as P
import Parsing.Combinators (choice, optionMaybe, try, manyTill) as P
import Parsing.Combinators.Array (many) as P

listP :: forall prefix p
  .  Parser prefix
  -> Parser p
  -> Parser (prefix /\ Array p)
-- listP prefix p = P.fail "x"
listP prefix p = P.indent *> ((/\) <$> prefix <*> P.indented_ (P.many p))

fencedP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
fencedP openP closeP p = do
    open <- P.indent *> openP <* P.nl
    body <- [] `flip tailRecM` \acc -> do
      P.optionMaybe (P.try (P.indent *> closeP <* P.nl')) >>= case _ of
        Nothing -> P.indent *> p <#> \a -> Loop (acc <> [a])
        Just _ -> pure $ Done acc
    pure (open /\ body)

multilineP :: forall p
  .  Parser Unit
  -> Parser p
  -> Parser (Array p)
multilineP prefix p
  =  P.indent 
  *> prefix
  *> P.indented
     (P.choice [prefix, P.indentation])
     (P.many p)

wrappedP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
wrappedP open close p = do
  a <- open
  bs <- List.toUnfoldable <$> P.manyTill p close
  pure (a /\ bs)
