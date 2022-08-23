module Editor.AST.Inline where

import Prelude

import Control.Monad.Free (Free)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Matryoshka (transAna)
import Matryoshka.Pattern.CoEnvT (CoEnvT(..))

data InlineKind = InlineK

derive instance genericInlineKind :: Generic InlineKind _
instance eqInlineKind :: Eq InlineKind where eq = genericEq
instance showInlineKind :: Show InlineKind where show = genericShow

newtype InlineF a b = InlineF (Tuple a (Array b))

derive instance newtypeInlineF :: Newtype (InlineF a r) _

instance functorInlineF :: Functor (InlineF a) where
  map f (InlineF r) = InlineF (map (map f) r)

instance bifunctorInlineF :: Bifunctor InlineF where
  bimap f g (InlineF (a /\ r)) = InlineF (f a /\ map g r)

instance eqInlineF :: ( Eq a, Eq r ) => Eq (InlineF a r) where
  eq (InlineF a) (InlineF b) = a == b

instance eq1InlineF :: Eq a => Eq1 (InlineF a) where
  eq1 (InlineF a) (InlineF b) = a == b

instance showInlineF :: ( Show a, Show b ) => Show (InlineF a b) where
  show (InlineF r) = "InlineF " <> show r

newtype Inline a b = Inline (Free (InlineF a) b)

derive instance newtypeInline :: Newtype (Inline a r) _
derive newtype instance eqInline :: ( Eq a, Eq r ) => Eq (Inline a r)

instance bifunctorInline :: Bifunctor Inline where
  bimap f g (Inline c0) = Inline $ transAna trans c0
    where
      trans (CoEnvT (Left b)) = CoEnvT (Left (g b))
      trans (CoEnvT (Right (InlineF (a /\ r)))) = CoEnvT (Right (InlineF (f a /\ r)))

instance functorInline :: Functor (Inline a) where
  map = bimap identity
