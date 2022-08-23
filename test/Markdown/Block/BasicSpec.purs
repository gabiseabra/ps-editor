module Test.Markdown.Syntax.BasicSpec where

import Markdown.AST.Inline (Inline(..), InlineF(..))
import Markdown.AST.Block (Block(..), BlockF(..))
import Markdown.Syntax.Basic (A(..), B(..), BasicBlockSyntax, BasicInlineSyntax, Blockquote(..), Code(..), H(..), HR(..), I(..), OL(..), P(..), S(..), UL(..), basicSyntax)
import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free (wrap)
import Control.Monad.Reader (runReader)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Data.Either.Inject (inj)
import Data.Maybe (Maybe(..))
import Data.String as String
import Markdown.Parser (emptyScope)
import Markdown.Syntax (markdownP)
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
  -> Array (Block BasicBlockSyntax (Inline BasicInlineSyntax String))
  -> m Unit
runTest md ast =
  let p = many (markdownP basicSyntax) <* eof
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
  [ Block $ inj H1 :< PureF [Inline $ pure "H1"]
  , Block $ inj H2 :< PureF [Inline $ pure "H2"]
  , Block $ inj H3 :< PureF [Inline $ pure "H3"]
  , Block $ inj H4 :< PureF [Inline $ pure "H4"]
  , Block $ inj H5 :< PureF [Inline $ pure "H5"]
  , Block $ inj H6 :< PureF [Inline $ pure "H6"]
  , Block $ inj P :< PureF [Inline $ pure "####### x"]
  , Block $ inj P :< PureF [Inline $ pure "#x"]
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
a *i* **b** ~~s~~

~~nested **inline *elements***~~

*this *isn't* nested*

this is [a link](href); [links can have **nested ~~elements~~ btw**](href)
""" `runTest` 
  [ Block $ inj P :< PureF
    [ Inline $ pure "a "
    , Inline $ wrap $ InlineF $ inj I /\ [pure "i"]
    , Inline $ pure " "
    , Inline $ wrap $ InlineF $ inj B /\ [pure "b"]
    , Inline $ pure " "
    , Inline $ wrap $ InlineF $ inj S /\ [pure "s"]
    ]
  , Block $ inj P :< PureF []
  , Block $ inj P :< PureF
    [ Inline $ wrap $ InlineF $ inj S /\
      [ pure "nested "
      , wrap $ InlineF $ inj B /\
        [ pure "inline "
        , wrap $ InlineF $ inj I /\ [pure "elements"]
        ]
      ]
    ]
  , Block $ inj P :< PureF []
  , Block $ inj P :< PureF
    [ Inline $ wrap $ InlineF $ inj I /\ [pure "this "]
    , Inline $ pure "isn't"
    , Inline $ wrap $ InlineF $ inj I /\ [pure " nested"]
    ]
  , Block $ inj P :< PureF []
  , Block $ inj P :< PureF
    [ Inline $ pure "this is "
    , Inline $ wrap $ InlineF $ inj (A "href") /\ [pure "a link"]
    , Inline $ pure "; "
    , Inline $ wrap $ InlineF $ inj (A "href") /\
      [ pure "links can have "
      , wrap $ InlineF $ inj B /\
        [ pure "nested "
        , wrap $ InlineF $ inj S /\ [pure "elements"]
        , pure " btw"
        ]
      ]
    ]
  ] :: Test

simpleCodeSpec = """
```
a
```
```code
b
```
""" `runTest` 
  [ Block $ inj (Code Nothing) :< TextF ["a"]
  , Block $ inj (Code (Just "code")) :< TextF ["b"]
  ] :: Test

simpleBlockquoteSpec = """
> a
  b

> # x
> - a
>   - b
""" `runTest` 
  [ Block $ inj Blockquote :< NestedF
    [ inj P :< PureF [Inline $ pure "a"]
    , inj P :< PureF [Inline $ pure "b"]
    ]
  , Block $ inj P :< PureF []
  , Block $ inj Blockquote :< NestedF
    [ inj H1 :< PureF [Inline $ pure "x"]
    , inj UL :< NestedF
      [ inj P :< PureF [Inline $ pure "a"]
      , inj UL :< NestedF
        [ inj P :< PureF [Inline $ pure "b"]
        ]
      ]
    ]
  ] :: Test

simpleUnorderedListSpec = """
- a
- b
  c
""" `runTest` 
  [ Block $ inj UL :< NestedF [ inj P :< PureF [Inline $ pure "a"] ]
  , Block $ inj UL :< NestedF
      [ inj P :< PureF [Inline $ pure "b"]
      , inj P :< PureF [Inline $ pure "c"]
      ]
  ] :: Test

simpleOrderedListSpec = """
1. a
2. b
420. c
""" `runTest` 
  [ Block $ inj (OL 1) :< (NestedF [ inj P :< PureF [Inline $ pure "a"] ])
  , Block $ inj (OL 2) :< (NestedF [ inj P :< PureF [Inline $ pure "b"] ])
  , Block $ inj (OL 420) :< (NestedF [ inj P :< PureF [Inline $ pure "c"] ])
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
    [ inj P :< PureF [Inline $ pure "a"]
    , inj UL :< (NestedF
      [ inj P :< PureF [Inline $ pure "b"]
      , inj P :< PureF [Inline $ pure "c"]
      ])
    , inj UL :< (NestedF
      [ inj (Code (Just "purs")) :< TextF ["code"]
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
