module Editor.Syntax.Basic where

import Editor.AST
import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((:))
import Data.Array.NonEmpty as NEArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Data.Tuple.Nested ((/\))
import Editor.Lexer as P
import Parsing as P
import Parsing.Combinators (optionMaybe) as P
import Parsing.Combinators.Array (many, many1) as P
import Parsing.String (string) as P
import Parsing.String.Basic (letter) as P
import Type.Proxy (Proxy(..))

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
  blockKind _ = Block
  formatBlock _ = map \a -> "- " <> a
  parseBlock p = (UL /\ _) <$> (P.string "- " *> P.many p)

data Code = Code (Maybe String)

derive instance genericCode :: Generic Code _
instance eqCode :: Eq Code where eq = genericEq
instance showCode :: Show Code where show = genericShow

instance blockTypeCode :: BlockType Code where
  blockKind _ = Text
  formatBlock _ as = ["```"] <> as <> ["```"]
  parseBlock p = do
    let langP =   map (String.fromCharArray <<< NEArray.toUnfoldable)
              <$> P.optionMaybe (P.many1 P.letter)
    lang <- P.string "```" *> langP <* P.nl
    body <- [] `flip tailRecM` \acc -> do
      P.optionMaybe (P.string "```" *> P.nl') >>= case _ of
        Nothing -> do
          p <#> \a -> Loop (a:acc)
        Just _ -> pure $ Done acc
    pure (Code lang /\ body)

type BasicSyntax = Code || UL || P

basicSyntax = Proxy :: Proxy BasicSyntax
