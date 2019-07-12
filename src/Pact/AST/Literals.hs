{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Pact Literal (builtin ground types)
--
module Pact.AST.Literals
( -- * Data
  Literal(..)
  -- * Prisms
, _LitInteger
, _LitDecimal
, _LitTime
, _LitList
, _LitString
, _LitBool
, _LitUint
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.Text
import Data.Word

type Decimal = Double
type Time = Double

data Literal a
  = LitInteger Integer
  | LitDecimal Decimal
  | LitTime    Time
  | LitList    [a]
  | LitObject  [(Text, a)]
  | LitString  {-# UNPACK #-} !Text
  | LitBool    {-# UNPACK #-} !Bool
  | LitUint    {-# UNPACK #-} !Word64
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Hashable, Generic, NFData)
makePrisms ''Literal
