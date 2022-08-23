module Editor.Syntax.Helpers where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (Either)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Editor.Parser (Parser)
import Editor.Parser (indent, indentP, indented, indented_, nl, nl') as P
import Parsing.Combinators (choice, optionMaybe, try, manyTill) as P
import Parsing.Combinators.Array (many) as P

infixr 6 type Either as ||

listBlockP :: forall prefix p
  .  Parser prefix
  -> Parser p
  -> Parser (prefix /\ Array p)
listBlockP prefix p = P.indentP *> ((/\) <$> prefix <*> P.indented_ (P.many p))

listBlockF :: String -> Array String -> Array String
listBlockF prefix as = case Array.uncons as of
  Just { head, tail } -> (prefix <> head) `Array.cons` tail
  _ -> [prefix]

fencedBlockP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
fencedBlockP openP closeP p = do
    open <- P.indentP *> openP <* P.nl
    body <- [] `flip tailRecM` \acc -> do
      P.optionMaybe (P.try (P.indentP *> closeP <* P.nl')) >>= case _ of
        Nothing -> P.indentP *> p <#> \a -> Loop (acc <> [a])
        Just _ -> pure $ Done acc
    pure (open /\ body)

fencedBlockF :: String -> String -> Array String -> Array String
fencedBlockF open close as = [open] <> as <> [close]

multilineBlockP :: forall p
  .  Parser Unit
  -> Parser p
  -> Parser (Array p)
multilineBlockP prefix p
  =  P.indentP 
  *> prefix
  *> P.indented
     (P.choice [prefix, P.indent])
     (P.many p)

multilineBlockF :: String -> Array String -> Array String
multilineBlockF prefix = map \a -> prefix <> a

wrappedInlineP :: forall open close p
  .  Parser open
  -> Parser close
  -> Parser p
  -> Parser (open /\ Array p)
wrappedInlineP open close p = do
  a <- open
  bs <- List.toUnfoldable <$> P.manyTill p close
  pure (a /\ bs)

wrappedInlineF :: String -> String -> Array String -> Array String
wrappedInlineF open close as = [open] <> as <> [close]