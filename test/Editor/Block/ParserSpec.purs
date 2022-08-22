module Test.Editor.Syntax.BasicSpec where

import Editor.Syntax.Basic
import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Either.Inject (inj)
import Data.Maybe (Maybe(..))
import Data.String as String
import Editor.Block.AST (Block(..), BlockF(..))
import Editor.Block.Parser (mkBlockParser)
import Effect.Exception (Error)
import Parsing (runParser)
import Parsing.Combinators.Array (many)
import Parsing.String (eof)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nestedList = """
- a
  - b\
    c
  - ```purs
    code
    ```
```
""" :: String

runTest :: forall m
  .  MonadThrow Error m
  => String
  -> Array (Block String BasicSyntax)
  -> m Unit
runTest md ast =
  runParser (String.trim md) (many (mkBlockParser basicSyntax) <* eof)
    `shouldEqual` Right ast

spec :: Spec Unit
spec =
  describe "mkBlockParser" do
    it "nestedList" $ runTest nestedList
      [ B $ inj UL :< (BlockF
        [ inj P :< PureF ["a\n"]
        , inj UL :< (BlockF
          [ inj P :< PureF ["b\nc\n"]
          ])
        , inj UL :< (BlockF
          [ inj (Code (Just "purs")) :< PureF ["code\n"]
          ])
        ])
      ]

