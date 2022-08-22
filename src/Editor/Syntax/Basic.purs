module Editor.Syntax.Basic where

import Editor.AST
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Parsing.Combinators.Array (many) as P
import Parsing.String (string) as P

data P = P

derive instance genericP :: Generic P _
instance eqP :: Eq P where eq = genericEq
instance showP :: Show P where show = genericShow

instance blockTypeP :: BlockType P where
  blockKind _ = Pure
  formatBlock _ = identity
  parseBlock p = (P /\ _) <$> map pure p

data UL = UL

derive instance genericUL :: Generic UL _
instance eqUL :: Eq UL where eq = genericEq
instance showUL :: Show UL where show = genericShow

instance blockTypeUL :: BlockType UL where
  blockKind _ = Pure
  formatBlock _ = map \a -> "- " <> a
  parseBlock p = (UL /\ _) <$> P.many (P.string "- " *> p)

type BasicSyntax = UL || P

