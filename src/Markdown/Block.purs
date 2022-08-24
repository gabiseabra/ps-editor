module Markdown.Block where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Control.Comonad.Env (EnvT(..))
import Control.Lazy (fix)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Eq (class Eq1)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Matryoshka (cata)

data BlockK
  = NestedK -- Blocks with nested blocks (UL, Blockquote)
  | PureK -- Blocks with inline elements (P)

foreign import data NestedK :: BlockK
foreign import data PureK :: BlockK

class IsBlockKind k where reflectBlock :: forall proxy. proxy k -> BlockK
instance IsBlockKind NestedK where reflectBlock _ = NestedK
instance IsBlockKind PureK where reflectBlock _ = PureK

data BlockF a r
  = NestedF (Array r)
  | PureF (Array a)

instance functorBlockF :: Functor (BlockF a) where
  map f (NestedF r) = NestedF (map f r)
  map _ (PureF a) = PureF a

instance bifunctorBlockF :: Bifunctor BlockF where
  bimap _ g (NestedF r) = NestedF (map g r)
  bimap f _ (PureF a) = PureF (map f a)

instance eqBlockF :: ( Eq a, Eq r ) => Eq (BlockF a r) where
  eq (NestedF a) (NestedF b) = a == b
  eq (PureF a) (PureF b) = a == b
  eq _ _ = false

instance eq1BlockF :: Eq a => Eq1 (BlockF a) where
  eq1 (NestedF a) (NestedF b) = a == b
  eq1 (PureF a) (PureF b) = a == b
  eq1 _ _ = false

instance showBlockF :: ( Show a, Show b ) => Show (BlockF a b) where
  show (NestedF r) = "NestedF " <> show r
  show (PureF a) = "PureF " <> show a

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

tag :: forall a b. Block a b -> a
tag (Block c) = head c

body :: forall a b. Block a b -> BlockF b (Block a b)
body (Block c) = map Block $ tail c
