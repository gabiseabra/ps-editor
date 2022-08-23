module Test.Editor.Syntax.BasicSpec where

import Editor.Syntax.Basic.Block
import Editor.AST.Block
import Editor.Lexer (emptyScope, line_)
import Editor.Syntax (blockP)
import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (runReader)
import Data.Either (Either(..))
import Data.Either.Inject (inj)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Parsing (runParserT)
import Parsing.Combinators.Array (many)
import Parsing.String (eof)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type Test = Aff Unit

runTest :: forall m
  .  MonadThrow Error m
  => String
  -> Array (Block BasicBlockSyntax String)
  -> m Unit
runTest md ast =
  let p = many (blockP Proxy line_) <* eof
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
  [ Block $ inj H1 :< PureF ["H1\n"]
  , Block $ inj H2 :< PureF ["H2\n"]
  , Block $ inj H3 :< PureF ["H3\n"]
  , Block $ inj H4 :< PureF ["H4\n"]
  , Block $ inj H5 :< PureF ["H5\n"]
  , Block $ inj H6 :< PureF ["H6\n"]
  , Block $ inj P :< PureF ["####### x\n"]
  , Block $ inj P :< PureF ["#x"]
  ] :: Test

simpleThematicBreakSpec = """
---
- - -
***
* * *
""" `runTest` 
  [ Block $ inj HR :< UnitF
  , Block $ inj HR :< UnitF
  , Block $ inj HR :< UnitF
  , Block $ inj HR :< UnitF
  ] :: Test

simpleParagraphSpec = """
a

b
""" `runTest` 
  [ Block $ inj P :< PureF ["a\n"]
  , Block $ inj P :< PureF ["\n"]
  , Block $ inj P :< PureF ["b"]
  ] :: Test

simpleCodeSpec = """
```
a
```
```code
b
```
""" `runTest` 
  [ Block $ inj (Code Nothing) :< TextF ["a\n"]
  , Block $ inj (Code (Just "code")) :< TextF ["b\n"]
  ] :: Test

simpleBlockquoteSpec = """
> a
  b

> # x
> - a
>   - b
""" `runTest` 
  [ Block $ inj Blockquote :< NestedF
    [ inj P :< PureF ["a\n"]
    , inj P :< PureF ["b\n"]
    ]
  , Block $ inj P :< PureF ["\n"]
  , Block $ inj Blockquote :< NestedF
    [ inj H1 :< PureF ["x\n"]
    , inj UL :< NestedF
      [ inj P :< PureF ["a\n"]
      , inj UL :< NestedF
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
  [ Block $ inj UL :< NestedF [ inj P :< PureF ["a\n"] ]
  , Block $ inj UL :< NestedF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c"]
      ]
  ] :: Test

simpleOrderedListSpec = """
1. a
2. b
420. c
""" `runTest` 
  [ Block $ inj (OL 1) :< (NestedF [ inj P :< PureF ["a\n"] ])
  , Block $ inj (OL 2) :< (NestedF [ inj P :< PureF ["b\n"] ])
  , Block $ inj (OL 420) :< (NestedF [ inj P :< PureF ["c"] ])
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
  [ Block $ inj UL :< (NestedF
    [ inj P :< PureF ["a\n"]
    , inj UL :< (NestedF
      [ inj P :< PureF ["b\n"]
      , inj P :< PureF ["c\n"]
      ])
    , inj UL :< (NestedF
      [ inj (Code (Just "purs")) :< TextF ["code\n"]
      , inj UL :< (NestedF
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
