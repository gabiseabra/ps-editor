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

simpleHeadingSpec = """
# H1
## H2
### H3
#### H4
##### H5
###### H6
####### x
#x
""" `runTest` 
  [ B $ inj H1 :< PureF ["H1\n"]
  , B $ inj H2 :< PureF ["H2\n"]
  , B $ inj H3 :< PureF ["H3\n"]
  , B $ inj H4 :< PureF ["H4\n"]
  , B $ inj H5 :< PureF ["H5\n"]
  , B $ inj H6 :< PureF ["H6\n"]
  , B $ inj P :< PureF ["####### x\n"]
  , B $ inj P :< PureF ["#x"]
  ] :: Test

simpleThematicBreakSpec = """
---
- - -
***
* * *
""" `runTest` 
  [ B $ inj HR :< UnitF
  , B $ inj HR :< UnitF
  , B $ inj HR :< UnitF
  , B $ inj HR :< UnitF
  ] :: Test

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

simpleBlockquoteSpec = """
> a
  b

> # x
> - a
>   - b
""" `runTest` 
  [ B $ inj Blockquote :< BlockF
    [ inj P :< PureF ["a\n"]
    , inj P :< PureF ["b\n"]
    ]
  , B $ inj P :< PureF ["\n"]
  , B $ inj Blockquote :< BlockF
    [ inj H1 :< PureF ["x\n"]
    , inj UL :< BlockF
      [ inj P :< PureF ["a\n"]
      , inj UL :< BlockF
        [ inj P :< PureF ["b"]
        ]
      ]
    ]
  ] :: Test

simpleUnorderedListSpec = """
- a
- b
  c
""" `runTest` 
  [ B $ inj UL :< BlockF [ inj P :< PureF ["a\n"] ]
  , B $ inj UL :< BlockF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c"]
      ]
  ] :: Test

simpleOrderedListSpec = """
1. a
2. b
420. c
""" `runTest` 
  [ B $ inj (OL 1) :< (BlockF [ inj P :< PureF ["a\n"] ])
  , B $ inj (OL 2) :< (BlockF [ inj P :< PureF ["b\n"] ])
  , B $ inj (OL 420) :< (BlockF [ inj P :< PureF ["c"] ])
  ] :: Test

nestedListSpec = """
- a
  - b
    c
  - ```purs
    code
    ```
    - * * *
""" `runTest` 
  [ B $ inj UL :< (BlockF
    [ inj P :< PureF ["a\n"]
    , inj UL :< (BlockF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c\n"]
      ])
    , inj UL :< (BlockF
      [ inj (Code (Just "purs")) :< TextF ["code\n"]
      , inj UL :< (BlockF
        [ inj HR :< UnitF
        ])
      ])
    ])
  ] :: Test

spec :: Spec Unit
spec =
  describe "mkBlockParser" do
    it "simpleHeading" simpleHeadingSpec
    it "simpleThematicBreak" simpleThematicBreakSpec
    it "simpleParagraph" simpleParagraphSpec
    it "simpleCode" simpleCodeSpec
    it "simpleBlockquote" simpleBlockquoteSpec
    it "simpleUnorderedList" simpleUnorderedListSpec
    it "simpleOrderedList" simpleOrderedListSpec
    it "nestedList" nestedListSpec
