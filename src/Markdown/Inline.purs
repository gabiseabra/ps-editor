module Markdown.Inline where

import Prelude

import Control.Monad.Free (Free, resume)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Matryoshka (cata, transAna)
import Matryoshka.Pattern.CoEnvT (CoEnvT(..))
import Data.Either.Nested (type (\/))

data InlineK

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
  bimap f g (Inline r) = Inline $ transAna trans r
    where
      trans (CoEnvT (Left b)) = CoEnvT (Left (g b))
      trans (CoEnvT (Right (InlineF (a /\ r')))) = CoEnvT (Right (InlineF (f a /\ r')))

instance functorInline :: Functor (Inline a) where
  map = bimap identity

instance showInline :: ( Show a, Show b ) => Show (Inline a b) where
  show (Inline r) = r `flip cata` alg
    where
      alg (CoEnvT (Left a)) = show a
      alg (CoEnvT (Right (InlineF (a /\ elements)))) =
        show a <> " [" <> String.joinWith "," elements <> "]"

unwrapInline :: forall a b.  Inline a b -> (a /\ Array (Inline a b)) \/ b
unwrapInline (Inline c) = case resume c of
  Left (InlineF (a /\ r)) -> Left (a /\ map Inline r)
  Right b -> Right b