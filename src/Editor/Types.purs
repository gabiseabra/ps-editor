module Editor.Types where

import Prelude

import Data.Generic.Rep (class Generic)

data Indent
  = IN
  | UL
  | OL Int

derive instance genericIndent :: Generic Indent _
derive instance eqIndent :: Eq Indent

instance showIndent :: Show Indent where
  show IN = "IN"
  show UL = "UL"
  show (OL n) = "OL " <> show n

data Block a
  = P a
  | BlockQuote a
  | Code a
  | Indented Indent (Block a)
  | HR

derive instance genericBlock :: Generic (Block a) _
derive instance eqBlock :: Eq a => Eq (Block a)
derive instance functorBlock :: Functor Block

instance showBlock :: Show a => Show (Block a) where
  show (P a) = "P " <> show a
  show (BlockQuote a) = "BlockQuote " <> show a
  show (Code a) = "Code " <> show a
  show (Indented i bs) = show i <> " " <> show bs
  show HR = "HR"

data Inline a
  = B a
  | I a
  | A String a

derive instance genericInline :: Generic (Inline a) _
derive instance eqInline :: Eq a => Eq (Inline a)
derive instance functorInline :: Functor Inline

instance showInline :: Show a => Show (Inline a) where
  show (B a) = "B " <> show a
  show (I a) = "I " <> show a
  show (A href a) = "A " <> href <> show a

