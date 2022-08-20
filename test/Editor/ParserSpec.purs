module Test.Editor.ParserSpec where

import Editor.Types
import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Editor.Parser (markdownP)
import Effect.Exception (Error)
import Parsing (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

nestedList = """
- a
  - b
  - ```
    code
    ```
""" :: String

runTest :: forall m. MonadThrow Error m => String -> Array (Block String) -> m Unit
runTest md ast =
  runParser (String.trim md) markdownP `shouldEqual` (Right $ Array.toUnfoldable ast)

spec :: Spec Unit
spec =
  describe "markdownP" do
    it "nestedList" $ runTest nestedList
      [ Indented UL
        [ P "a"
        , Indented UL
          [ P "b"
          , Code "code"
          ]
        ]
      ]
