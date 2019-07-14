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
, BindSort
  -- * Prisms
, _Literal
, _Var
, _App
, _Lam
, _Let
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
import Pact.Names
import Pact.Types

data BindSort = Rec | NonRec
  deriving Show

-- | 'Expr' values are the result of compilation, after the type
-- inference/checking process with types checked and stripped
--
data Expr n a
  = Literal SourceSpan (Literal a)
    -- ^ literal expressions with source pos
  | Var SourceSpan n
    -- ^ individual variable expressions with source pos
  | App a (Expr n a) (Expr n a)
    -- ^ expression application
  | Lam a BasicName (Expr n a)
    -- ^ lambda abstraction expressions (defun, defcap, defpact)
  | Let BindSort (NonEmpty (Expr n a)) (Expr n a)
    -- ^ recursive let expressions (separate from above for perf reasons)
  | IfThenElse (Expr n a) (Expr n a) (Expr n a)
    -- ^ if/then/else trees
  | PositionalValue SourceSpan [Comment] (Expr n a)
    -- ^ some value at some source position
  | Accessor {-# UNPACK #-} !Text (Expr n a)
    -- ^ object accessors a la x.y
  | ObjectUpdate (Expr n a) (NonEmpty (Text, (Expr n a)))
    -- ^ object update expressions `(update x { y : foo })`
  | TypedValue {-# UNPACK #-} !Text (Expr n a) (Type n a)
    -- ^ typed value expression
  | Anonymous
    -- ^ anonymous placeholder
  | Hole {-# UNPACK #-} !Text
    -- ^ hole expressions
  deriving Show

makePrisms ''Expr
