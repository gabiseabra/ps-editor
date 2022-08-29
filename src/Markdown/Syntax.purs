module Markdown.Syntax
  ( AST
  , class Element
  , parse
  , BlockP
  , class IsBlock
  , mkBlockP
  , class BlockCompiler
  , mkBlockParser
  , class InlineCompiler
  , mkInlineParser
  , markdownP
  , parseMarkdown
  , module Markdown.AST
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Env (EnvT(..))
import Control.Lazy (fix)
import Control.Monad.Free (Free, resume)
import Control.Monad.Reader (runReader)
import Data.Array (foldr)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import Data.String (fromCodePointArray)
import Data.Tuple.Nested (type (/\), (/\))
import Markdown.AST (Block, Inline)
import Markdown.Parser (Parser)
import Markdown.Parser (emptyScope) as P
import Matryoshka (embed)
import Parsing (ParseError, runParserT, fail) as P
import Parsing.Combinators (try) as P
import Parsing.Combinators.Array (many) as P
import Parsing.String (anyCodePoint, eof) as P
import Type.Proxy (Proxy(..))

type AST block inline = Array (Block block (Array (Inline inline String)))

class Element :: (Type -> Type -> Type) -> Type -> Constraint
class Element f a | a -> f where
  parse :: forall b r. Parser (f b r) -> Parser (a /\ Array (f b r))

data BlockP b
  = NestedP (forall r
    .  Parser (Block b r)
    -> Parser (Block b r)
    )
  | FlatP (forall i r
    .  Parser (Inline i r)
    -> Parser (Block b (Array (Inline i r)))
    )

class IsBlock :: (Type -> Type -> Type) -> Constraint
class IsBlock p where
  mkBlockP :: forall proxy a b.  Element p a => proxy p -> (a -> b) -> BlockP b
instance IsBlock Block where
  mkBlockP _ f = NestedP \p -> embed <<< EnvT <<< bimap f (Compose <<< Right) <$> parse p
instance IsBlock Inline where
  mkBlockP _ f = FlatP \p -> embed <<< EnvT <<< bimap f (Compose <<< Left) <$> parse p

class BlockCompiler a where
  mkBlockParser :: forall b i r
    .  (a -> b)
    -> Parser (Inline i r)
    -> Parser (Block b (Array (Inline i r)))
    -> Parser (Block b (Array (Inline i r)))
instance blockCompilerVoid :: BlockCompiler Void where
  mkBlockParser _ _ _ = P.fail "Void"
else instance blockCompilerEither ::
  ( BlockCompiler l
  , BlockCompiler r
  ) => BlockCompiler (Either l r) where
  mkBlockParser f i b = P.try l <|> r
    where l = mkBlockParser (f <<< Left) i b
          r = mkBlockParser (f <<< Right) i b
else instance blockCompilerBlock ::
  ( Element p a
  , IsBlock p
  ) => BlockCompiler a where
  mkBlockParser f i b = case mkBlockP (Proxy :: Proxy p) f of
    NestedP g -> g b
    FlatP g -> g i

blockP :: forall b i r. BlockCompiler b => Parser (Inline i r) -> Parser (Block b (Array (Inline i r)))
blockP = fix <<< mkBlockParser identity

class InlineCompiler a where
  mkInlineParser :: forall i r
    . ((a /\ Array (Inline i r)) -> (i /\ Array (Inline i r)))
    -> Parser (Inline i r)
    -> Parser (Inline i r)
instance inlineCompilerVoid :: InlineCompiler Void where
  mkInlineParser _ _ = P.fail "Void"
else instance inlineCompilerEither ::
  ( InlineCompiler l
  , InlineCompiler r
  ) => InlineCompiler (Either l r) where
  mkInlineParser f i = P.try l <|> r
    where l = mkInlineParser (bimap Left identity >>> f) i
          r = mkInlineParser (bimap Right identity >>> f) i
else instance inlineCompilerInline ::
  ( Element Inline a
  ) => InlineCompiler a where
  mkInlineParser f i = embed <<< Newtype.wrap <<< Right <<< Compose <<< f <$> parse i

inlineP :: forall a. InlineCompiler a => Parser (Inline a String)
inlineP = fix \f -> P.try (nodeP f) <|> leafP
  where nodeP = mkInlineParser $ bimap identity (modify2 collapse)
        leafP = Newtype.wrap <<< pure <<< fromCodePointArray <<< pure <$> P.anyCodePoint

markdownP :: forall block inline
  .  BlockCompiler block
  => InlineCompiler inline
  => Parser (Block block (Array (Inline inline String)))
markdownP = map (map (modify2 collapse)) $ blockP inlineP

parseMarkdown :: forall block inline
  .  BlockCompiler block
  => InlineCompiler inline
  => String
  -> P.ParseError \/ (AST block inline)
parseMarkdown i = runReader (P.runParserT i p) P.emptyScope
  where p = P.many markdownP <* P.eof

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

collapse :: forall f m
  .  Functor f
  => Semigroup m
  => Array (Free f m)
  -> Array (Free f m)
collapse = foldr appendFreeM []

modify2 :: forall f a t. Newtype t a => Functor f => (f a -> f a) -> f t -> f t
modify2 f = map Newtype.wrap <<< f <<< map Newtype.unwrap
