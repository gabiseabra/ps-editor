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

listBlockP :: forall prefix p
  .  Parser prefix
  -> Parser p
  -> Parser (prefix /\ Array p)
listBlockP prefix p = P.indent *> ((/\) <$> prefix <*> P.indented_ (P.many p))

fencedBlockP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
fencedBlockP openP closeP p = do
    open <- P.indent *> openP <* P.nl
    body <- [] `flip tailRecM` \acc -> do
      P.optionMaybe (P.try (P.indent *> closeP <* P.nl')) >>= case _ of
        Nothing -> P.indent *> p <#> \a -> Loop (acc <> [a])
        Just _ -> pure $ Done acc
    pure (open /\ body)

multilineBlockP :: forall p
  .  Parser Unit
  -> Parser p
  -> Parser (Array p)
multilineBlockP prefix p
  =  P.indent 
  *> prefix
  *> P.indented
     (P.choice [prefix, P.indentation])
     (P.many p)

wrappedInlineP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
wrappedInlineP open close p = do
  a <- open
  bs <- List.toUnfoldable <$> P.manyTill p close
  pure (a /\ bs)
