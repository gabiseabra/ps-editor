module Editor.Syntax where

import Editor.AST
import Editor.AST.Block
import Editor.AST.Inline
import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Control.Lazy (fix)
import Control.Monad.Free (wrap)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.String (fromCodePointArray)
import Data.Tuple.Nested ((/\))
import Editor.Lexer (Parser, line_)
import Parsing.Combinators (try)
import Parsing.Combinators.Array (many)
import Parsing.String (anyCodePoint)
import Type.Proxy (Proxy(..))

infixr 6 type Either as ||

data Syntax :: Type -> Type -> Type
data Syntax block inline = Syntax

mergeSyntax :: forall a b c d. Syntax a b -> Syntax c d -> Syntax (a || c) (b || d)
mergeSyntax _ _ = Syntax

infixr 6 mergeSyntax as ||

class SyntaxCompiler k f a | k -> f where
  mkParser :: forall proxy b r
    .  proxy k
    -> (a -> b)
    -> Parser r
    -> Parser (f b r)
    -> Parser (f b r)
  mkFormatter :: forall proxy
    .  proxy k
    -> a
    -> Array String
    -> Array String

instance syntaxCompilerEither ::
  ( SyntaxCompiler k f a
  , SyntaxCompiler k f b
  ) => SyntaxCompiler k f (Either a b) where
  mkParser k f r p = try a <|> b
    where a = mkParser k (f <<< Left) r p
          b = mkParser k (f <<< Right) r p
  mkFormatter k (Left a) = mkFormatter k a
  mkFormatter k (Right b) = mkFormatter k b
else instance syntaxCompilerBlock ::
  ( Element BlockKind a
  ) => SyntaxCompiler BlockKind Block a where
  mkFormatter _ = format
  mkParser _ f r = case kind (Proxy :: Proxy a) of
    NestedK -> \p -> parse p <#> \(a /\ b) -> Block $ f a :< NestedF (map unwrap b)
    PureK -> \_ -> parse r <#> \(a /\ b) -> Block $ f a :< PureF b
    TextK -> \_ -> parse line_ <#> \(a /\ b) -> Block $ f a :< TextF b
    UnitK -> \_ -> parse (pure unit) <#> \(a /\ _) -> Block $ f a :< UnitF
else instance syntaxCompilerInline ::
  ( Element InlineKind a
  ) => SyntaxCompiler InlineKind Inline a where
  mkFormatter _ = format
  mkParser _ f r p = parse p' <#> \(a /\ b) -> Inline $ wrap (InlineF (f a /\ map unwrap b))
    where p' = try p <|> (Inline <<< pure <$> r)

leafP = fromCodePointArray <$> many anyCodePoint

blockP :: forall proxy a r
  .  SyntaxCompiler BlockKind Block a
  => proxy a
  -> Parser r
  -> Parser (Block a r)
blockP _ r = fix $ mkParser (Proxy :: Proxy BlockKind) identity r

inlineP :: forall proxy a r
  .  SyntaxCompiler InlineKind Inline a
  => proxy a
  -> Parser r
  -> Parser (Inline a r)
inlineP _ r = fix $ mkParser (Proxy :: Proxy InlineKind) identity r

markdownP :: forall block inline
  .  SyntaxCompiler BlockKind Block block
  => SyntaxCompiler InlineKind Inline inline
  => Syntax block inline
  -> Parser (Block block (Inline inline String))
markdownP _ = blockP Proxy $ inlineP Proxy leafP
