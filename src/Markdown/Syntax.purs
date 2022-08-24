module Markdown.Syntax
  ( AST
  , class Element
  , parse
  , class BlockCompiler
  , mkBlockParser
  , class InlineCompiler
  , mkInlineParser
  , markdownP
  , parseMarkdown
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (head, tail, (:<))
import Control.Lazy (fix)
import Control.Monad.Free (Free, resume, wrap)
import Control.Monad.Reader (runReader)
import Data.Array (foldr)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Newtype as Newtype
import Data.String (fromCodePointArray)
import Data.Tuple.Nested (type (/\), (/\))
import Markdown.Block (class IsBlockKind, Block(..), BlockF(..), BlockK(..), reflectBlock)
import Markdown.Inline (Inline(..), InlineF(..), InlineK)
import Markdown.Parser (Parser, emptyScope)
import Parsing (ParseError, runParserT)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String (anyCodePoint, eof)
import Type.Proxy (Proxy(..))

type AST block inline = Array (Block block (Inline inline String))

class Element :: forall k. k -> Type -> Constraint
class Element k a | a -> k where
  parse :: forall r. Parser r -> Parser (a /\ Array r)
  -- | @TODO
  -- format :: a -> Array String -> Array String

class BlockCompiler a where
  mkBlockParser :: forall r b
    . (a -> b)
    -> Parser r
    -> Parser (Block b r)
    -> Parser (Block b r)

instance blockCompilerEither ::
  ( BlockCompiler a
  , BlockCompiler b
  ) => BlockCompiler (Either a b) where
  mkBlockParser f r p = try a <|> b
    where a = mkBlockParser (f <<< Left) r p
          b = mkBlockParser (f <<< Right) r p
else instance blockCompilerElement ::
  ( Element k a
  , IsBlockKind k
  ) => BlockCompiler a where
  mkBlockParser f r = case reflectBlock (Proxy :: Proxy k) of
    NestedK -> \p -> parse p <#> \(a /\ b) -> Block $ f a :< NestedF (map unwrap b)
    PureK -> \_ -> parse r <#> \(a /\ b) -> Block $ f a :< PureF b

class InlineCompiler a where
  mkInlineParser :: forall b
    . (a -> b)
    -> Parser (Inline b String)
    -> Parser (Inline b String)

instance inlineCompilerEither ::
  ( InlineCompiler a
  , InlineCompiler b
  ) => InlineCompiler (Either a b) where
  mkInlineParser f p = try a <|> b
    where a = mkInlineParser (f <<< Left) p
          b = mkInlineParser (f <<< Right) p
else instance inlineCompilerInline ::
  ( Element InlineK a
  ) => InlineCompiler a where
  mkInlineParser f p =
    parse p <#> \(a /\ b) ->
      Inline $ wrap (InlineF (f a /\ collapse (map unwrap b)))

blockP :: forall a r.  BlockCompiler a => Parser r -> Parser (Block a r)
blockP r = fix $ mkBlockParser identity r

inlineP :: forall a. InlineCompiler a => Parser (Inline a String)
inlineP = fix \f -> try (nodeP f) <|> leafP
  where nodeP = mkInlineParser identity
        leafP = Inline <<< pure <<< fromCodePointArray <<< pure <$> anyCodePoint

markdownP :: forall block inline
  .  BlockCompiler block
  => InlineCompiler inline
  => Parser (Block block (Inline inline String))
markdownP = map f $ blockP inlineP
  where
    f a = a `flip modify` \c -> head c :< (g (modify2 collapse)) (tail c)
    -- g :: forall x a b. (Array a -> Array b) -> BlockF a x -> BlockF b x
    g h (PureF as) = PureF $ h as
    g _ (NestedF a) = NestedF a

parseMarkdown :: forall block inline
  .  BlockCompiler block
  => InlineCompiler inline
  => String
  -> Either ParseError (Array (Block block (Inline inline String)))
parseMarkdown i = runReader (runParserT i p) emptyScope
  where p = many markdownP <* eof

--------------------------------------------------------------------------------

appendFreeM :: forall f m
  .  Functor f
  => Semigroup m
  => Free f m
  -> Array (Free f m)
  -> Array (Free f m)
appendFreeM a as = case (resume a /\ uncons' as) of
    (Right x /\ Just (Right x' /\ xs')) -> pure (x <> x') `Array.(:)` xs'
    _ -> a `Array.(:)` as
  where uncons' = Array.uncons >>> map \{ head, tail } -> (resume head /\ tail) 

collapse :: forall f
  .  Functor f
  => Array (Free f String)
  -> Array (Free f String)
collapse = foldr appendFreeM []

modify2 :: forall f a t. Newtype t a => Functor f => (f a -> f a) -> f t -> f t
modify2 f = map Newtype.wrap <<< f <<< map Newtype.unwrap
