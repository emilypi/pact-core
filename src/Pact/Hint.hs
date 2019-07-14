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

data ErrorHint
  = ErrorUnifyingTypes (Type SourceAnn) (Type SourceAnn)
  | ErrorInExpression SExpr
  | ErrorInModule ModuleName
  | ErrorInSubsumption (Type SourceAnn) (Type SourceAnn)
  | ErrorCheckingAccessor SExpr {-# UNPACK #-} !Text
  | ErrorCheckingType SExpr (Type SourceAnn)
  | ErrorCheckingKind (Type SourceAnn)
  | ErrorInferringType SExpr
  | ErrorInApplication SExpr (Type SourceAnn) SExpr
  | ErrorInBindingGroup (NonEmpty Ident)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | PositionedError (NonEmpty SourceSpan)
  deriving (Show)
