module Pact.Hint
( ErrorHint(..)
) where


import Data.List.NonEmpty
import Data.Text

import Pact.AST.SourcePos
import Pact.Expr
import Pact.Names
import Pact.Types

type SExpr = Expr SourceAnn
type NamedType = Type BasicName SourceAnn

data ErrorHint
  = ErrorUnifyingTypes NamedType NamedType
  | ErrorInExpression SExpr
  | ErrorInModule ModuleName
  | ErrorInSubsumption NamedType NamedType
  | ErrorCheckingAccessor SExpr {-# UNPACK #-} !Text
  | ErrorCheckingType SExpr NamedType
  | ErrorCheckingKind NamedType
  | ErrorInferringType SExpr
  | ErrorInApplication SExpr NamedType SExpr
  | ErrorInBindingGroup (NonEmpty Ident)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | PositionedError (NonEmpty SourceSpan)
  deriving (Show)
