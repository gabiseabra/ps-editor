module Test.Editor.Syntax.BasicSpec where

import Editor.Syntax.Basic
import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (runReader)
import Data.Either (Either(..))
import Data.Either.Inject (inj)
import Data.Maybe (Maybe(..))
import Data.String as String
import Editor.Block.AST (Block(..), BlockF(..))
import Editor.Block.Parser (mkBlockParser)
import Editor.Lexer (emptyScope)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Parsing (runParserT)
import Parsing.Combinators.Array (many)
import Parsing.String (eof)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Test = Aff Unit

runTest :: forall m
  .  MonadThrow Error m
  => String
  -> Array (Block String BasicSyntax)
  -> m Unit
runTest md ast =
  let p = many (mkBlockParser basicSyntax) <* eof
  in runReader (runParserT (String.trim md) p) emptyScope
      `shouldEqual` Right ast

simpleParagraphSpec = """
a

b
""" `runTest` 
  [ B $ inj P :< PureF ["a\n"]
  , B $ inj P :< PureF ["\n"]
  , B $ inj P :< PureF ["b"]
  ] :: Test

simpleCodeSpec = """
```
a
```
```code
b
```
""" `runTest` 
  [ B $ inj (Code Nothing) :< TextF ["a\n"]
  , B $ inj (Code (Just "code")) :< TextF ["b\n"]
  ] :: Test

simpleListSpec = """
- a
- b
  c
""" `runTest` 
  [ B $ inj UL :< (BlockF [ inj P :< PureF ["a\n"] ])
  , B $ inj UL :< (BlockF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c"]
      ])
  ] :: Test

nestedListSpec = """
- a
  - b
    c
  - ```purs
    code
    ```
""" `runTest` 
  [ B $ inj UL :< (BlockF
    [ inj P :< PureF ["a\n"]
    , inj UL :< (BlockF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c\n"]
      ])
    , inj UL :< (BlockF
      [ inj (Code (Just "purs")) :< TextF ["code\n"]
      ])
    ])
  ] :: Test

spec :: Spec Unit
spec =
  describe "mkBlockParser" do
    it "simpleParagraph" simpleParagraphSpec
    it "simpleCode" simpleCodeSpec
    it "simpleList" simpleListSpec
    it "nestedList" nestedListSpec
