module Editor.AST
  ( module Editor.Block.AST
  , type (||)
  ) where

import Editor.Block.AST

import Data.Either (Either)

infixr 6 type Either as ||
