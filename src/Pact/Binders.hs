{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- AST binder data types
--
module Pact.Binders
( Binder(..)
  -- * Prisms
, _LiteralBinder
, _VarBinder
, _NamedBinder
, _TypedBinder
  -- * Combinators
, binderNames
) where


import Control.Lens

import Data.HashMap.Strict hiding (foldl')
import Data.Foldable (foldl')

import Pact.AST.Literals
import Pact.AST.SourcePos
import Pact.Names
import Pact.Types


data Binder
  = LiteralBinder SourceSpan !(Literal Binder)
    -- ^ Literal bindings at some span
  | VarBinder SourceSpan !Ident
    -- ^ Variable bindings at some span
  | NamedBinder SourceSpan Ident Binder
    -- ^ Bind an input to an identifier
  | PositionalBinder SourceSpan [Comment] Binder
    -- ^ Source positional binder
  | TypedBinder (Type SourceAnn) Binder
    -- ^ Binder with type annotation
  deriving Show

makePrisms ''Binder

instance Eq Binder where
  LiteralBinder _ l == LiteralBinder _ l' = l == l'
  VarBinder _ i == VarBinder _ i' = i == i'
  NamedBinder _ i b == NamedBinder _ i' b' = i == i' && b == b'
  PositionalBinder _ c b == PositionalBinder _ c' b' =
    c == c' && b == b'
  TypedBinder ty b == TypedBinder ty' b' = ty == ty' && b == b'
  _ == _ = False

instance Ord Binder where
  compare (LiteralBinder _ l) (LiteralBinder _ l') = l `compare` l'
  compare LiteralBinder{} _ = LT
  compare (VarBinder _ i) (VarBinder _ i') = i `compare` i'
  compare VarBinder{} LiteralBinder{} = GT
  compare VarBinder{} _ = LT
  compare (NamedBinder _ i b) (NamedBinder _ i' b') = i `compare` i' <> b `compare` b'
  compare NamedBinder{} PositionalBinder{} = LT
  compare NamedBinder{} TypedBinder{} = LT
  compare NamedBinder{} _ = GT
  compare (PositionalBinder _ cs b) (PositionalBinder _ cs' b') = cs `compare` cs' <> b `compare` b
  compare PositionalBinder{} TypedBinder{} = LT
  compare PositionalBinder{} _ = GT
  compare (TypedBinder ty b) (TypedBinder ty' b') = ty `compare` ty' <> b `compare` b'
  compare TypedBinder{} _ = GT


binderNames :: Binder -> [Ident]
binderNames = go []
  where
    go l = \case
      LiteralBinder _ b -> lit l b
      VarBinder _ n -> n:l
      NamedBinder _ n b -> go (n:l) b
      PositionalBinder _ _ b -> go l b
      TypedBinder _ b -> go l b

    lit l = \case
      LitObject bs -> foldl' go l (fmap snd $ toList bs)
      LitList l' -> foldl' go l l'
      _ -> l
