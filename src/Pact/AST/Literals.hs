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
, _LitObject
) where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.HashMap.Strict
import Data.Text
import Data.Word


type Decimal = Double
type Time = Double

data Literal a
  = LitInteger Integer
    -- ^ Arbitrary precision integer literals
  | LitDecimal Decimal
    -- ^ 256-figure integer.mantissa-flavored decimals
  | LitTime    Time
    -- ^ UTC-time literals
  | LitList    [a]
    -- ^ Haskell list literals
  | LitObject  (HashMap Text a)
    -- ^ Object (row) literals as hashmaps of labels and values
  | LitString  {-# UNPACK #-} !Text
    -- ^ Literal UTF-8 encoded text strings
  | LitBool    {-# UNPACK #-} !Bool
    -- ^ Literal boolean values
  | LitUint    {-# UNPACK #-} !Word64
    -- ^ Unsigned 64-bit word literals
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Hashable, Generic, NFData)
makePrisms ''Literal
