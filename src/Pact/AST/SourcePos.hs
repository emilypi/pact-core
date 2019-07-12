{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- Source position tracking
--
module Pact.AST.SourcePos
( -- * Data
  SourcePos(..)
, SourceSpan(..)
  -- * Types
, Comment
, SourceAnn
  -- * Combinators
, ppSourcePos
, ppSourcePosShort
, ppStartEnd
, ppStartEndShort
, ppSourceSpan
, initSourceSpan
, initSourceAnn
  -- * Patterns
, pattern EmptyPos
, pattern EmptySpan
, pattern EmptyAnn
)where


import GHC.Generics

import Control.DeepSeq

import Data.Text
import Data.Word

import System.FilePath

import Pact.Utils

type Comment = Text
type SourceAnn = (SourceSpan, [Comment])


data SourcePos = SourcePos
  { _posLine :: {-# UNPACK #-} !Int
  , _posColumn :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord, Show, Generic, NFData)

instance Semigroup SourcePos where
  (SourcePos l c) <> (SourcePos l' c') = SourcePos (l + l') (c + c')

instance Monoid SourcePos where
  mempty = SourcePos 0 0

data SourceSpan = SourceSpan
  { _spanName :: {-# UNPACK #-} !Text
  , _spanStart :: !SourcePos
  , _spanEnd :: !SourcePos
  } deriving (Eq, Ord, Show, Generic, NFData)

ppSourcePos :: SourcePos -> Text
ppSourcePos (SourcePos l c) = "line: " <> tshow l <> ", column: " <> tshow c

ppSourcePosShort :: SourcePos -> Text
ppSourcePosShort (SourcePos l c) = tshow l <> ":" <> tshow c

ppStartEnd :: SourceSpan -> Text
ppStartEnd (SourceSpan _ s e) = "(" <> (ppSourcePos s) <> " - " <> (ppSourcePos e) <> ")"

ppStartEndShort :: SourceSpan -> Text
ppStartEndShort (SourceSpan _ s e) = ppSourcePosShort s <> "-" <> ppSourcePosShort e

ppSourceSpan :: FilePath -> SourceSpan -> Text
ppSourceSpan fp s = pack (makeRelative fp $ unpack (_spanName s))
  <> ":"
  <> ppStartEndShort s
  <> " "
  <> ppStartEnd s

initSourceSpan :: Text -> SourceSpan
initSourceSpan n = SourceSpan n (SourcePos 0 0) (SourcePos 0 0)

initSourceAnn :: SourceAnn
initSourceAnn = (initSourceSpan "", [])

pattern EmptyPos :: SourcePos
pattern EmptyPos = SourcePos 0 0

pattern EmptySpan :: SourceSpan
pattern EmptySpan = SourceSpan "" EmptyPos EmptyPos

pattern EmptyAnn :: SourceAnn
pattern EmptyAnn = (EmptySpan, [])


widenSpan :: SourceSpan -> SourceSpan -> SourceSpan
widenSpan EmptySpan b = b
widenSpan a EmptySpan = a
widenSpan (SourceSpan n s e) (SourceSpan n' s' e')
  | n == "" = SourceSpan n' (min s s') (max e e')
  | otherwise = SourceSpan n (min s s') (max e e')
