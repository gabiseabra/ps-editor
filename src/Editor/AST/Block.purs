module Editor.AST.Block where

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
import Data.String as String
import Data.Tuple.Nested ((/\))
import Matryoshka (cata)

data BlockKind
  = NestedK -- Blocks with nested blocks (UL, Blockquote)
  | PureK -- Blocks with inline elements (P)
  | TextK -- Blocks with pre-formatted text (Code)
  | UnitK -- Blocks with no content (HR)

derive instance genericBlockKind :: Generic BlockKind _
instance eqBlockKind :: Eq BlockKind where eq = genericEq
instance showBlockKind :: Show BlockKind where show = genericShow

data BlockF a r
  = NestedF (Array r)
  | PureF (Array a)
  | TextF (Array String)
  | UnitF

instance functorBlockF :: Functor (BlockF a) where
  map f (NestedF r) = NestedF (map f r)
  map _ (PureF a) = PureF a
  map _ (TextF a) = TextF a
  map _ UnitF = UnitF

instance bifunctorBlockF :: Bifunctor BlockF where
  bimap _ g (NestedF r) = NestedF (map g r)
  bimap f _ (PureF a) = PureF (map f a)
  bimap _ _ (TextF a) = TextF a
  bimap _ _ UnitF = UnitF

instance eqBlockF :: ( Eq a, Eq r ) => Eq (BlockF a r) where
  eq (NestedF a) (NestedF b) = a == b
  eq (PureF a) (PureF b) = a == b
  eq (TextF a) (TextF b) = a == b
  eq UnitF UnitF = true
  eq _ _ = false

instance eq1BlockF :: Eq a => Eq1 (BlockF a) where
  eq1 (NestedF a) (NestedF b) = a == b
  eq1 (PureF a) (PureF b) = a == b
  eq1 (TextF a) (TextF b) = a == b
  eq1 UnitF UnitF = true
  eq1 _ _ = false

instance showBlockF :: ( Show a, Show b ) => Show (BlockF a b) where
  show (NestedF r) = "NestedF " <> show r
  show (PureF a) = "PureF " <> show a
  show (TextF a) = "TextF " <> show a
  show UnitF = "UnitF"

newtype Block a b = Block (Cofree (BlockF b) a)

derive instance newtypeBlock :: Newtype (Block a r) _
derive newtype instance eqBlock :: ( Eq a, Eq r ) => Eq (Block a r)

instance bifunctorBlock :: Bifunctor Block where
  bimap f g (Block c0) = Block $ c0 `flip fix` \h c -> f (head c) :< bimap g h (tail c)

instance functorBlock :: Functor (Block a) where
  map = bimap identity

instance showBlock :: ( Show a, Show b ) => Show (Block a b) where
  show (Block c) = c `flip cata` \(EnvT (r /\ block)) ->
    show r <> ":<" <> (
      case block of
        (NestedF bs) -> "NestedF " <> "[" <> String.joinWith "," bs <> "]"
        b -> show b
    )
