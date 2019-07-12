{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- AST binder data types
--
module Pact.Expr
( -- * Data
  Expr(..)
  -- * Prisms
, _Literal
, _Var
, _App
, _Lam
, _Let
, _LetRec
, _IfThenElse
, _PositionalValue
, _Accessor
, _ObjectUpdate
, _TypedValue
, _Anonymous
, _Hole
)where


import Control.Lens

import Data.Text

import Pact.AST.Literals
import Pact.AST.SourcePos
import Pact.Names
import Pact.Types



type Declaration = Int
type Binder = Int

data Expr
  = Literal SourceSpan (Literal Expr)
    -- ^ literal expressions with source pos
  | Var SourceSpan Ident
    -- ^ individual variable expressions with source pos
  | App Expr Expr
    -- ^ expression application
  | Lam Binder Expr
    -- ^ lambda abstraction expressions (defun, defcap, defpact)
  | Let Declaration Expr
    -- ^ let expressions
  | LetRec [Declaration] Expr
    -- ^ recursive let expressions (separate from above for perf reasons)
  | IfThenElse Expr Expr Expr
    -- ^ if/then/else trees
  | PositionalValue SourceSpan [Comment] Expr
    -- ^ some value at some source position
  | Accessor {-# UNPACK #-} !Text Expr
    -- ^ object accessors a la x.y
  | ObjectUpdate Expr [(Text, Expr)]
    -- ^ object update expressions `(update x { y : foo })`
  | TypedValue {-# UNPACK #-} !Bool Expr (Type SourceAnn)
    -- ^ typed value expressions
  | Anonymous
    -- ^ anonymous placeholder
  | Hole {-# UNPACK #-} !Text
    -- ^ hole expressions
  deriving Show

makePrisms ''Expr
