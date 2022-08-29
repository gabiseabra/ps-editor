module Test.Markdown.Syntax.BasicSpec where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.Either.Inject (inj)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Markdown.AST (block, inline)
import Markdown.Syntax (AST, parseMarkdown)
import Markdown.Syntax.Basic (A(..), B(..), BasicBlockSyntax, BasicInlineSyntax, Blockquote(..), Code(..), H(..), HR(..), I(..), OL(..), P(..), S(..), UL(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Test = Aff Unit

type BasicAST = AST BasicBlockSyntax BasicInlineSyntax

runTest :: forall m
  .  MonadThrow Error m
  => String
  -> BasicAST
  -> m Unit
runTest md ast = parseMarkdown (String.trim md) `shouldEqual` Right ast

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
  [ block $ inj H1 /\ Left [ pure "H1" ]
  , block $ inj H2 /\ Left [ pure "H2" ]
  , block $ inj H3 /\ Left [ pure "H3" ]
  , block $ inj H4 /\ Left [ pure "H4" ]
  , block $ inj H5 /\ Left [ pure "H5" ]
  , block $ inj H6 /\ Left [ pure "H6" ]
  , block $ inj P /\ Left [ pure "####### x" ]
  , block $ inj P /\ Left [ pure "#x" ]
  ] :: Test

simpleThematicBreakSpec = """
---
- - -
***
* * *
""" `runTest` 
  [ block $ inj HR /\ Left []
  , block $ inj HR /\ Left []
  , block $ inj HR /\ Left []
  , block $ inj HR /\ Left []
  ] :: Test

simpleParagraphSpec = """
*italic* **bold** ~~strike~~

~~nested **inline *elements***~~

*this *isn't* nested*

this is [a link](href); [links can have **nested ~~elements~~ btw**](href)
""" `runTest` 
  [ block $ inj P /\ Left
    [ inline $ inj I /\ [ pure "italic" ]
    , pure " "
    , inline $ inj B /\ [ pure "bold" ]
    , pure " "
    , inline $ inj S /\ [ pure "strike" ]
    ]
  , block $ inj P /\ Left []
  , block $ inj P /\ Left
    [ inline $ inj S /\
      [ pure "nested "
      , inline $ inj B /\
        [ pure "inline "
        , inline $ inj I /\ [ pure "elements" ]
        ]
      ]
    ]
  , block $ inj P /\ Left []
  , block $ inj P /\ Left
    [ inline $ inj I /\ [ pure "this " ]
    , pure "isn't"
    , inline $ inj I /\ [ pure " nested" ]
    ]
  , block $ inj P /\ Left []
  , block $ inj P /\ Left
    [ pure "this is "
    , inline $ inj (A "href") /\ [ pure "a link" ]
    , pure "; "
    , inline $ inj (A "href") /\
      [ pure "links can have "
      , inline $ inj B /\
        [ pure "nested "
        , inline $ inj S /\ [pure "elements"]
        , pure " btw"
        ]
      ]
    ]
  ] :: Test

simpleCodeSpec = """
```
a
b
```
```code
c
```
""" `runTest` 
  [ block $ inj (Code Nothing ["a", "b"]) /\ Left []
  , block $ inj (Code (Just "code") ["c"]) /\ Left []
  ] :: Test

simpleBlockquoteSpec = """
> a
  b

> # x
> - a
>   - b
""" `runTest` 
  [ block $ inj Blockquote /\ Right
    [ block $ inj P /\ Left [ pure "a" ]
    , block $ inj P /\ Left [ pure "b" ]
    ]
  , block $ inj P /\ Left []
  , block $ inj Blockquote /\ Right
    [ block $ inj H1 /\ Left [ pure "x" ]
    , block $ inj UL /\ Right
      [ block $ inj P /\ Left [ pure "a" ]
      , block $ inj UL /\ Right
        [ block $ inj P /\ Left [ pure "b" ]
        ]
      ]
    ]
  ] :: Test

simpleUnorderedListSpec = """
- a
- b
  c
""" `runTest` 
  [ block $ inj UL /\ Right
    [ block $ inj P /\ Left [ pure "a" ]
    ]
  , block $ inj UL /\ Right
      [ block $ inj P /\ Left [ pure "b" ]
      , block $ inj P /\ Left [ pure "c" ]
      ]
  ] :: Test

simpleOrderedListSpec = """
1. a
2. b
420. c
""" `runTest` 
  [ block $ inj (OL 1) /\ Right
    [ block $ inj P /\ Left [ pure "a" ]
    ]
  , block $ inj (OL 2) /\ Right
    [ block $ inj P /\ Left [ pure "b" ]
    ]
  , block $ inj (OL 420) /\ Right
    [ block $ inj P /\ Left [ pure "c" ]
    ]
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
  [ block $ inj UL /\ Right
    [ block $ inj P /\ Left [ pure "a" ]
    , block $ inj UL /\ Right
      [ block $ inj P /\ Left [ pure "b" ]
      , block $ inj P /\ Left [ pure "c" ]
      ]
    , block $ inj UL /\ Right
      [ block $ inj (Code (Just "purs") ["code"]) /\ Left []
      , block $ inj UL /\ Right 
        [ block $ inj HR /\ Left []
        ]
      ]
    ]
  ] :: Test

spec :: Spec Unit
spec =
  describe "basic syntax" do
    it "simpleHeading" simpleHeadingSpec
    it "simpleThematicBreak" simpleThematicBreakSpec
    it "simpleParagraph" simpleParagraphSpec
    it "simpleCode" simpleCodeSpec
    it "simpleBlockquote" simpleBlockquoteSpec
    it "simpleUnorderedList" simpleUnorderedListSpec
    it "simpleOrderedList" simpleOrderedListSpec
    it "nestedList" nestedListSpec
