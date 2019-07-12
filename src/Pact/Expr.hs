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
, _Value
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
  | Var SourceSpan Ident
  | App Expr Expr
  | Lam Binder Expr
  | Let Declaration Expr
  | LetRec [Declaration] Expr
  | IfThenElse Expr Expr Expr
  | Value SourceSpan [Comment] Expr
  | Accessor {-# UNPACK #-} !Text Expr
  | ObjectUpdate Expr [(Text, Expr)]
  | TypedValue {-# UNPACK #-} !Bool Expr (Type SourceAnn)
  | Anonymous
  | Hole {-# UNPACK #-} !Text
  deriving Show

makePrisms ''Expr
