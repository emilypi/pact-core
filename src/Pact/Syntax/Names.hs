{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
module Pact.Syntax.Names
( -- * Data
  Ident(..)
, NameSort(..)
, RawModule(..)
, ResolvedModule(..)
, Namespace(..)
, FullyQualified(..)
, NameOrigin(..)
, BasicName(..)
  -- * Optics
, rawModule
, resolvedModule
, namespace
, fqName
, fqModule
, fqNamesort
, fqNamespace
, bName
, bModule
, bOrigin
, bNameSpace
  -- * Combinators
, ppIdent
) where


import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.Hashable
import Data.Text
import Data.Word


import Pact.Syntax.Utils

data Ident
  = Ident {-# UNPACK #-} !Text
    -- ^ Alphanumeric identifiers
  | GenIdent !(Maybe Text) {-# UNPACK #-} !Int
    -- ^ Generated names/de bruijn indices
  | UnusedIdent
  deriving (Show, Eq, Ord, Hashable, Generic)

instance NFData Ident

makePrisms ''Ident

ppIdent :: Ident -> Text
ppIdent (Ident t) = t
ppIdent (GenIdent Nothing t) = "$" <> tshow t
ppIdent (GenIdent (Just n) t) = "$" <> n <> tshow t
ppIdent UnusedIdent = "$~unused"


data NameOrigin
  = LocalOrigin
  | TopLevelOrigin
  | BuiltInOrigin
  | ImportedOrigin
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

data NameSort
  = Private
  | Public
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

data NameVisibility
  = Undefined
  | Defined
  deriving (Eq, Ord, Show, Generic, Hashable, NFData)

newtype Namespace = Namespace { _namespace :: Text }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

namespace :: Lens' Namespace Text
namespace = lens _namespace (\t b -> t { _namespace = b })

newtype RawModule = RawModule { _rawModule :: Text }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

rawModule :: Lens' RawModule Text
rawModule = lens _rawModule (\t b -> t { _rawModule = b })

newtype ResolvedModule = ResolvedModule { _resolvedModule :: Text }
  deriving (Eq, Ord, Show, Generic, NFData, Hashable)

resolvedModule :: Lens' ResolvedModule Text
resolvedModule = lens _resolvedModule (\t b -> t { _resolvedModule = b })


data BasicName = BasicName
  { _bName :: !Ident
  , _bNameSpace :: !Namespace
  , _bOrigin :: !NameOrigin
  , _bModule :: !RawModule
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''BasicName

data FullyQualified = FullyQualified
  { _fqName :: {-# UNPACK #-} !Text
  , _fqUnique :: {-# UNPACK #-} !Int
  , _fqNamespace :: {-# UNPACK #-} !Namespace
  , _fqModule :: {-# UNPACK #-} !ResolvedModule
  , _fqNamesort :: !NameSort
  } deriving (Eq, Ord, Show, Hashable, Generic, NFData)
makeLenses ''FullyQualified
