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
module Pact.Syntax.Kind
( -- * Data
  Kind(..)
  -- * Prisms
, _KType
, _KArrow
, _KRow
, _KHole
, _KConstraint
, _KCapability
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens
import Data.Text


data Kind a
  = KType a
    -- ^ The kind of concrete types (i.e. * or Type)
  | KArrow a (Kind a) (Kind a)
    -- ^ The kind of kind-level arrows Type -> Type
  | KRow a [Kind a]
    -- ^ The kind of row-types
  | KHole a {-# UNPACK #-} !Int
    -- ^ The kind of unknown kinds Kind : Kind - used as for unification
  | KConstraint a
    -- ^ The kind of functor constraints of modules
  | KCapability a
    -- ^ The kind of security primitives a la capabilities
  deriving (Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)
makePrisms ''Kind

instance Show a => Show (Kind a) where
  show = \case
    KType _ -> "Type"
    KArrow _ t u -> show t <> " -> " <> show u
    KHole _ i -> "_" <> show i
    KConstraint _ -> "Constraint"
    KRow _ r -> show $ fmap show r
    KCapability _ -> "Capability"
