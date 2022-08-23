module Editor.AST where

import Prelude

import Editor.Parser (Parser)
import Data.Tuple.Nested (type (/\))

class Element k a | a -> k where
  kind :: forall proxy. proxy a -> k
  parse :: forall r. Parser r -> Parser (a /\ Array r)
  format :: a -> Array String -> Array String
