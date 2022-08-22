module Editor.Block.Parser where

import Editor.Block.AST
import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Editor.Lexer (Parser, line_)
import Type.Proxy (Proxy(..))


class BlockDef a where
  blockParser :: forall b
    .  (a -> b)
    -> { blockP :: Parser (Block' String b)
        , pureP :: Parser String
        , textP :: Parser String
        , unitP :: Parser Unit
        }
    -> Parser (Block' String b)
  blockFormatter :: a -> Array String -> Array String

instance blockDefEither :: ( BlockDef a, BlockDef b ) => BlockDef (Either a b) where
  blockParser f p = blockParser (f <<< Left) p <|> blockParser (f <<< Right) p
  blockFormatter (Left a) = blockFormatter a
  blockFormatter (Right b) = blockFormatter b
else instance blockDefType :: BlockType a => BlockDef a where
  blockParser f = case blockKind (Proxy :: Proxy a) of
    Block -> \{ blockP } -> parseBlock blockP <#> \(a /\ b) -> f a :< BlockF b
    Pure -> \{ pureP } -> parseBlock pureP <#> \(a /\ b) -> f a :< PureF b
    Unit -> \{ unitP } -> parseBlock unitP <#> \(a /\ _) -> f a :< UnitF
  blockFormatter = formatBlock

mkBlockParser :: forall a. BlockDef a => Proxy a -> Parser (Block String a)
mkBlockParser _ =
  let p = blockParser identity
      pureP = line_
      textP = line_
      unitP = pure unit
  in B <$> fix \blockP -> p { blockP, pureP, textP, unitP }
