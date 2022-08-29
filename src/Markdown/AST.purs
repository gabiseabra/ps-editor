module Markdown.AST where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<), head, tail)
import Control.Comonad.Env (EnvT(..))
import Control.Lazy (fix)
import Control.Monad.Free (Free)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Functor.Compose (Compose(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Matryoshka (class Corecursive, class Recursive, cata, embed, project, transAna)
import Matryoshka.Pattern.CoEnvT (CoEnvT(..))

type InlineF a = Compose (Tuple a) Array -- = EnvT a Array

newtype Inline a b = Inline (Free (InlineF a) b)

inline' :: forall a b. (b \/ (a /\ Array (Inline a b))) -> Inline a b
inline' = embed <<< CoEnvT <<< bimap identity Compose

inline :: forall a b. (a /\ Array (Inline a b)) -> Inline a b
inline = inline' <<< Right

derive instance newtypeInline :: Newtype (Inline a b) _
derive newtype instance eqInline :: ( Eq a, Eq b ) => Eq (Inline a b)
instance showInline :: ( Show a, Show b ) => Show (Inline a b) where
  show (Inline r) = r `flip cata` alg
    where
      alg (CoEnvT (Left a)) = "Leaf " <> show a
      alg (CoEnvT (Right (Compose (a /\ elements)))) =
        "Inline " <> show a <> " [" <> String.joinWith "," elements <> "]"
instance bifunctorInline :: Bifunctor Inline where
  bimap f g (Inline r) = Inline $ transAna trans r
    where
      trans (CoEnvT (Left b)) = CoEnvT (Left (g b))
      trans (CoEnvT (Right (Compose (a /\ r')))) = CoEnvT (Right (Compose (f a /\ r')))
instance functorInline :: Functor (Inline a) where
  map = bimap identity
instance applyInline :: Apply (Inline a) where
  apply (Inline f) (Inline a) = Inline (apply f a)
instance applicativeInline :: Applicative (Inline a) where
  pure = Inline <<< pure
instance Recursive (Inline a b) (CoEnvT b (InlineF a)) where
    project = map Inline <<< project <<< unwrap
instance Corecursive (Inline a b) (CoEnvT b (InlineF a)) where
    embed = Inline <<< embed <<< map unwrap

--------------------------------------------------------------------------------

type BlockF a = Compose (Either a) Array -- = CoEnvT a Array

newtype Block a b = Block (Cofree (BlockF b) a)

block :: forall a b. (a /\ (b \/ Array (Block a b))) -> Block a b
block = embed <<< EnvT <<< bimap identity Compose

derive instance newtypeBlock :: Newtype (Block a b) _
derive newtype instance eqBlock :: ( Eq a, Eq b ) => Eq (Block a b)
instance showBlock :: ( Show a, Show b ) => Show (Block a b) where
  show (Block r) = r `flip cata` \(EnvT (a /\ b)) ->
    show a <> ":<" <> case b of
      (Compose (Left as)) -> "Flat " <> show as
      (Compose (Right bs)) -> "Nested " <> "[" <> String.joinWith "," bs <> "]"
instance functorBlock :: Functor (Block a) where map = bimap identity
instance bifunctorBlock :: Bifunctor Block where
  bimap f g (Block r) = Block $ r `flip fix` \h c -> f (head c) :< case tail c of
    (Compose (Left as)) -> Compose $ Left $ g as
    (Compose (Right bs)) -> Compose $ Right $ map h bs
instance Recursive (Block a b) (EnvT a (BlockF b)) where
    project = map Block <<< project <<< unwrap
instance Corecursive (Block a b) (EnvT a (BlockF b)) where
    embed = Block <<< embed <<< map unwrap
