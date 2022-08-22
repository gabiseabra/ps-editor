module Editor.Block.Parser where

import Editor.Block.AST
import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Lazy (fix)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant as Var
import Editor.Internal.RL (RL)
import Editor.Internal.RL as RL
import Editor.Lexer (Parser)
import Parsing as P
import Prim.Row as Row
import Type.RowList as RL

data FoldBlockParser (rl :: RL.RowList Type) = FoldBlockParser

instance foldrFoldBlockParser ::
  ( IsSymbol k
  , RL.RowListGet rl k (BlockDef a)
  , BlockTypes rl rl'
  , RL.ListToRow rl' r
  , Row.Cons k a r' r
  ) => RL.FoldrRL (FoldBlockParser rl) k (BlockDef a)
                  (Parser (Block String r) -> Parser (Block String r)) where
  stepFoldrRL _ k (BlockDef { kind, parse }) cont = \p -> cont p <|> do
    (a /\ body) <- case kind of
                    _ -> parse p <#> \(a /\ b) -> (a /\ BlockF b)
    pure (Var.inj k a :< body)

blockP :: forall defs rl r a
  .  BlockTypes defs rl
  => RL.ListToRow rl r
  => RL.RunFoldrRL (FoldBlockParser defs) a (Parser (Block String r) -> Parser (Block String r)) defs defs
  => RL defs
  -> Parser (Block String r)
blockP defs = fix $ RL.foldrRL f (\_ -> P.fail "No block parsers") defs
  where f = FoldBlockParser :: FoldBlockParser defs
