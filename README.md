A composable, extensible purescript markdown parser.
Define custom elements with applicative parsers from purescript-parsing,
  or use one of the combinators exported from `Markdown.Syntax.Helpers`
  to use common patterns such as fenced blocks (code blocks)
  and prefixed multiline blocks (blockquote).

## Syntax Definitions

All markdown elements have to define their own implementation of a parser by
  declaring an instance of the `Element` type-class. This class is parametrized by:

- `a` — A `Type` representing a type of markdown element;
- `k` — Either a type of kind `BlockK` for block-level elements,
        or the type `InlineK` for line-level elements;

```purs
class Element k a | a -> k where
  -- | A parser of an element `a` containing many children `r` extracted from a
  -- | parser of individual tokens `r`. The type of tokens depends on `k`.
  parse :: forall r. Parser r -> Parser (a /\ Array r)
```

Both block- and inline-level element types are represented by a nested `Either`
  ADT with all leaves instances of `Element`.
So extending syntax definitions is as simple as declaring an instance of
  `Element` for a custom ADT and nesting it on top of the original syntax:

```purs
data X = X

instance Element NestedK X where parse p = pure (X, [])

type MySyntax = X \/ BasicBlockSyntax
```

Parsers are combined from left to right, so the order in which elements are
  arranged in the syntax definition matters. They must be in the correct order
  as to evaluate parsers from least to most general.
For example, the P element is the most general block-level parser because it
  doesn't have any opening or closing tags — it just interprets the remainder of
  the line as a string of inline elements — so it has to be the rightmost element
  in the syntax type definition: `(UL \/ OL \/ P)`.


## Example

Here is an example code which extends the basic syntax with
  custom inline elements and parses a markdown string.

```purs
import Data.Tuple.Nested ((/\))
import Data.Either.Nested (type (\/))
import Markdown.Inline (InlineK)
import Markdown.Syntax (class Element, AST, parseMarkdown)
import Markdown.Syntax.Basic (BasicBlockSyntax, BasicInlineSyntax)
import Markdown.Syntax.Helpers (wrappedInlineP) as S
import Markdown.Parser (word) as P
import Parsing.String (char) as P
import Parsing (ParseError)

data At = At String

instance showAt :: Show At where show (At a) = "At " <> show a

-- | An @mention element which consumes one "@" followed by one word. It stores
-- | the word in the ADT rather than parsing it as children because I want the
-- | value to be plain string (not an arbitrary inline element with possibly
-- | many nesting levels) which I can use later to query against a database.
instance Element InlineK At where parse _ = (_ /\ []) <<< At <$> (P.char '@' *> P.word)

data Sup = Sup

instance showSup :: Show Sup where show _ = "Sup"

-- | An element representing an html `sup` tag which consumes nested elements
-- | between two `^` characters.
instance Element InlineK Sup where parse p = S.wrappedInlineP (P.char '^' $> Sup) (P.char '^') p

-- Extends the basic syntax with our custom inline elements
type MyAST = AST BasicBlockSyntax (Sup \/ At \/ BasicInlineSyntax)

parseMarkdown "@gabiseabra ^test^" :: ParseError \/ MyAST
-- prints: (Right [(Right (Right (Right (Right (Right (Right P)))))):<PureF [(Right (Left At "gabiseabra")) []," ",(Left Sup) ["test"]]])
```

## TODO

- [] Extended and maybe github flavoured markdown syntaxes
- [] Add a common parser for html tag elements
- [] Add parser info to elements' definitions such as indentation and start position
- [] Make formatters
- [] Limit the maximum amount of nesting
- [] Export, import and validate ASTs to/from typescript
- [] Allow 4 space indentation
- [] Test more edge cases
- [] P.word minds unclosed tags
- [] Make special tokens escapable
