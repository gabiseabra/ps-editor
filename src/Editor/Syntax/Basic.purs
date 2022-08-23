module Editor.Syntax.Basic where

import Editor.Syntax.Basic.Block
import Editor.Syntax.Basic.Inline
import Editor.Syntax (Syntax(..))
import Prelude

import Type.Proxy (Proxy(..))

basicSyntax = Syntax :: Syntax BasicBlockSyntax BasicInlineSyntax
