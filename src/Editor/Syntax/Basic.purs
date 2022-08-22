module Editor.Syntax.Basic where

import Editor.Block.AST
import Editor.Internal.RL ((#:), (<>#))
import Prelude

import Data.Tuple.Nested ((/\))
import Parsing.Combinators.Array (many) as P
import Parsing.String (string) as P
import Type.Proxy (Proxy(..))

pBlock = BlockDef
  { kind: Pure
  , format: \_ as -> as
  , parse: \p -> (unit /\ _) <$> map pure p
  } :: BlockDef Unit

ulBlock = BlockDef
  { kind: Block
  , format: \_ -> map \a -> "- " <> a
  , parse: \p -> (unit /\ _) <$> P.many (P.string "- " *> p)
  } :: BlockDef Unit

basicSyntax
  =   (Proxy :: Proxy "ul") #: ulBlock
  <># (Proxy :: Proxy "p") #: pBlock
