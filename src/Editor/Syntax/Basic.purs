module Editor.Syntax.Basic where

import Editor.AST
import Prelude

import Data.Tuple.Nested ((/\))
import Parsing.Combinators.Array (many) as P
import Parsing.String (string) as P

data P = P

instance blockTypeP :: BlockType P where
  blockKind _ = Pure
  formatBlock _ = identity
  parseBlock p = (P /\ _) <$> map pure p

data UL = UL

instance blockTypeUL :: BlockType UL where
  blockKind _ = Pure
  formatBlock _ = map \a -> "- " <> a
  parseBlock p = (UL /\ _) <$> P.many (P.string "- " *> p)

type BasicSyntax = UL || P

