module Editor.Block.AST where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Control.Comonad.Env (EnvT(..))
import Control.Lazy (fix)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Eq (class Eq1)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Editor.Lexer (Parser)
import Matryoshka (cata)

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
  | PureF (Array a)
  | TextF (Array String)
  | UnitF

instance functorBlockF :: Functor (BlockF a) where
  map f (BlockF r) = BlockF (map f r)
  map _ (PureF a) = PureF a
  map _ (TextF a) = TextF a
  map _ UnitF = UnitF

instance bifunctorBlockF :: Bifunctor BlockF where
  bimap _ g (BlockF r) = BlockF (map g r)
  bimap f _ (PureF a) = PureF (map f a)
  bimap _ _ (TextF a) = TextF a
  bimap _ _ UnitF = UnitF

instance eqBlockF :: ( Eq a, Eq r ) => Eq (BlockF a r) where
  eq (BlockF a) (BlockF b) = a == b
  eq (PureF a) (PureF b) = a == b
  eq (TextF a) (TextF b) = a == b
  eq UnitF UnitF = true
  eq _ _ = false

instance eq1BlockF :: Eq a => Eq1 (BlockF a) where
  eq1 (BlockF a) (BlockF b) = a == b
  eq1 (PureF a) (PureF b) = a == b
  eq1 (TextF a) (TextF b) = a == b
  eq1 UnitF UnitF = true
  eq1 _ _ = false

instance showBlockF :: ( Show a, Show b ) => Show (BlockF a b) where
  show (BlockF r) = "BlockF " <> show r
  show (PureF a) = "PureF " <> show a
  show (TextF a) = "TextF " <> show a
  show UnitF = "UnitF"

type Block' a r = Cofree (BlockF a) r
newtype Block a r = B (Block' a r)

derive instance newtypeBlock :: Newtype (Block a r) _
derive newtype instance functorBlock :: Functor (Block a)
derive newtype instance eqBlock :: ( Eq a, Eq r ) => Eq (Block a r)

instance bifunctorBlock :: Bifunctor Block where
  bimap f g (B c0) = B $ c0 `flip fix` \h c -> g (head c) :< bimap f h (tail c)

instance showBlock :: ( Show a, Show r ) => Show (Block a r) where
  show (B c) = c `flip cata` \(EnvT (r /\ body)) -> show r <> ":<(" <> show body <> ")"

class BlockType a where
  blockKind :: forall proxy. proxy a -> BlockKind
  parseBlock :: forall r. Parser r -> Parser (a /\ Array r)
  formatBlock :: a -> Array String -> Array String
