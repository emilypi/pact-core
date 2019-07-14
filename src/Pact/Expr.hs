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
) where


import Control.Lens

import Data.List.NonEmpty
import Data.Text

import Pact.AST.Literals
import Pact.AST.SourcePos
import Pact.Declaration
import Pact.Names
import Pact.Types

data Expr a
  = Literal SourceSpan (Literal a)
    -- ^ literal expressions with source pos
  | Var SourceSpan Ident
    -- ^ individual variable expressions with source pos
  | App a (Expr a) (Expr a)
    -- ^ expression application
  | Fun a Ident (Expr a)
    -- ^ lambda abstraction expressions (defun, defcap, defpact)
  | Let (Declaration a) (Expr a)
    -- ^ let expressions
  | LetRec (NonEmpty (Declaration a)) (Expr a)
    -- ^ recursive let expressions (separate from above for perf reasons)
  | IfThenElse (Expr a) (Expr a) (Expr a)
    -- ^ if/then/else trees
  | PositionalValue SourceSpan [Comment] (Expr a)
    -- ^ some value at some source position
  | Accessor {-# UNPACK #-} !Text (Expr a)
    -- ^ object accessors a la x.y
  | ObjectUpdate (Expr a) (NonEmpty (Text, (Expr a)))
    -- ^ object update expressions `(update x { y : foo })`
  | TypedValue {-# UNPACK #-} !Text (Expr a) (Type SourceAnn)
    -- ^ typed value expression
  | Anonymous
    -- ^ anonymous placeholder
  | Hole {-# UNPACK #-} !Text
    -- ^ hole expressions
  deriving Show

makePrisms ''Expr
