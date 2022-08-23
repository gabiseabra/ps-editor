module Markdown.Syntax where

import Markdown.AST
import Markdown.AST.Block
import Markdown.AST.Inline
import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (head, tail, (:<))
import Control.Lazy (fix)
import Control.Monad.Free (Free, resume, wrap)
import Data.Array (foldr)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, modify, unwrap)
import Data.Newtype as Newtype
import Data.String (fromCodePointArray)
import Data.Tuple.Nested ((/\))
import Markdown.Parser (Parser)
import Markdown.Parser as P
import Parsing.Combinators (try)
import Parsing.String (anyCodePoint)
import Type.Proxy (Proxy(..))

infixr 6 type Either as ||

data Syntax :: Type -> Type -> Type
data Syntax block inline = Syntax

mergeSyntax :: forall a b c d. Syntax a b -> Syntax c d -> Syntax (a || c) (b || d)
mergeSyntax _ _ = Syntax

infixr 6 mergeSyntax as ||

appendFreeM :: forall f m
  .  Functor f
  => Semigroup m
  => Free f m
  -> Array (Free f m)
  -> Array (Free f m)
appendFreeM a as = case (resume a /\ uncons' as) of
    (Right a /\ Just (Right a' /\ as)) -> pure (a <> a') `Array.(:)` as
    _ -> a `Array.(:)` as
  where uncons' = Array.uncons >>> map \{ head, tail } -> (resume head /\ tail) 

collapse :: forall f
  .  Functor f
  => Array (Free f String)
  -> Array (Free f String)
collapse = foldr appendFreeM []

modify2 :: forall f a t. Newtype t a => Functor f => (f a -> f a) -> f t -> f t
modify2 f = map Newtype.wrap <<< f <<< map Newtype.unwrap

class FormatterCompiler a where
  mkFormatter :: a -> Array String -> Array String
instance formatterCompilerEither ::
  ( FormatterCompiler a
  , FormatterCompiler b
  ) => FormatterCompiler (Either a b) where
  mkFormatter (Left a) = mkFormatter a
  mkFormatter (Right b) = mkFormatter b
else instance formatterCompilerElement :: ( Element k a ) => FormatterCompiler a where
  mkFormatter = format

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
  ( Element BlockKind a
  ) => BlockCompiler a where
  mkBlockParser f r = case kind (Proxy :: Proxy a) of
    NestedK -> \p -> parse p <#> \(a /\ b) -> Block $ f a :< NestedF (map unwrap b)
    PureK -> \_ -> parse r <#> \(a /\ b) -> Block $ f a :< PureF b
    TextK -> \_ -> parse P.lineP <#> \(a /\ b) -> Block $ f a :< TextF b
    UnitK -> \_ -> parse (pure unit) <#> \(a /\ _) -> Block $ f a :< UnitF

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
  ( Element InlineKind a
  ) => InlineCompiler a where
  mkInlineParser f p =
    parse p <#> \(a /\ b) ->
      Inline $ wrap (InlineF (f a /\ collapse (map unwrap b)))

blockP :: forall proxy a r
  .  BlockCompiler a
  => proxy a
  -> Parser r
  -> Parser (Block a r)
blockP _ r = fix $ mkBlockParser identity r

inlineP :: forall proxy a
  .  InlineCompiler a
  => proxy a
  -> Parser (Inline a String)
inlineP _ = fix \f -> try (nodeP f) <|> leafP
  where nodeP = mkInlineParser identity
        leafP = Inline <<< pure <<< fromCodePointArray <<< pure <$> anyCodePoint

markdownP :: forall block inline
  .  BlockCompiler block
  => InlineCompiler inline
  => Syntax block inline
  -> Parser (Block block (Inline inline String))
markdownP _ = map f $ blockP Proxy (inlineP Proxy)
  where
    f a = a `flip modify` \c -> head c :< (g (modify2 collapse)) (tail c)
    -- g :: forall x a b. (Array a -> Array b) -> BlockF a x -> BlockF b x
    g f (PureF as) = PureF $ f as
    g _ (NestedF a) = NestedF a
    g _ (TextF a) = TextF a
    g _ UnitF = UnitF
