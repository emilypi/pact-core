{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>, Emmanuel Denloye-Ito <emmanul@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Pact Keysets
--
module Pact.AST.KeySet where

import GHC.Generics

import Control.DeepSeq
import Control.Lens

import Data.ByteString (ByteString)
import Data.Set.NonEmpty (NESet)
import Data.Text

newtype KeySetPredicate = KeysetPredicate
  { _getPredicate :: Text
  } deriving (Eq, Ord, Show, Generic, NFData)

newtype PublicKey = PublicKey
  { _getPublicKey :: ByteString
  } deriving (Eq, Ord, Show, Generic, NFData)

data KeySet = KeySet
  { _ksPublicKeys :: NESet PublicKey
  , _ksPredicate :: {-# UNPACK #-} !KeySetPredicate
  } deriving (Eq, Ord, Show, Generic, NFData)

makeLenses ''KeySetPredicate
makeLenses ''PublicKey
makeLenses ''KeySet
