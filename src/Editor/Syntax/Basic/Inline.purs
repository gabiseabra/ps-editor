module Editor.Syntax.Basic.Inline where


import Editor.AST
import Editor.AST.Block
import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Editor.Lexer as P
import Editor.Syntax as S
import Editor.Syntax (type (||))

data Span = Span String

derive instance genericSpan :: Generic Span _
instance eqSpan :: Eq Span where eq = genericEq
instance showSpan :: Show Span where show = genericShow

data B = B String

derive instance genericB :: Generic B _
instance eqB :: Eq B where eq = genericEq
instance showB :: Show B where show = genericShow

data I = I String

derive instance genericI :: Generic I _
instance eqI :: Eq I where eq = genericEq
instance showI :: Show I where show = genericShow

data S = S String

derive instance genericS :: Generic S _
instance eqS :: Eq S where eq = genericEq
instance showS :: Show S where show = genericShow

data A = A String String

derive instance genericA :: Generic A _
instance eqA :: Eq A where eq = genericEq
instance showA :: Show A where show = genericShow


type BasicInlineSyntax = A || B || S || I
