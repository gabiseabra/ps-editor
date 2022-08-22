module Editor.Block.AST where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Bifunctor (class Bifunctor)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Editor.Lexer (Parser)
import Type.RowList as RL

data BlockKind
  = Block
  | Pure
  | Text
  | Unit

derive instance genericBlockKind :: Generic BlockKind _
instance eqBlockKind :: Eq BlockKind where eq = genericEq
instance showBlockKind :: Show BlockKind where show = genericShow

data BlockF a r
  = BlockF (Array r)
  | PureF a
  | TextF String
  | UnitF

instance functorBlockF :: Functor (BlockF a) where
  map f (BlockF r) = BlockF (map f r)
  map _ (PureF a) = PureF a
  map _ (TextF a) = TextF a
  map _ UnitF = UnitF

instance bifunctorBlockF :: Bifunctor BlockF where
  bimap _ g (BlockF r) = BlockF (map g r)
  bimap f _ (PureF a) = PureF (f a)
  bimap _ _ (TextF a) = TextF a
  bimap _ _ UnitF = UnitF

type Block a r = Cofree (BlockF a) (Variant r)

data BlockDef a = BlockDef
  { kind :: BlockKind
  , format :: a -> Array String -> Array String
  , parse :: forall r. Parser r -> Parser (a /\ Array r)
  }

class BlockTypes (defs :: RL.RowList Type) (rl :: RL.RowList Type) | defs -> rl
instance blockTypesCons ::
  ( BlockTypes defs rl
  ) => BlockTypes (RL.Cons k (BlockDef a) defs) (RL.Cons k a rl)
instance blockTypesNil :: BlockTypes RL.Nil RL.Nil
