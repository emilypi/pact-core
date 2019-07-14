module Pact.Hint where


import Data.List.NonEmpty
import Data.Text

import Pact.AST.SourcePos
import Pact.Expr
import Pact.Names
import Pact.Types

type Expr = Expr SourceAnn

data ErrorHint
  = ErrorUnifyingTypes (Type SourceAnn) (Type SourceAnn)
  | ErrorInExpression Expr
  | ErrorInModule ModuleName
  | ErrorInSubsumption (Type SourceAnn) (Type SourceAnn)
  | ErrorCheckingAccessor Expr {-# UNPACK #-} !Text
  | ErrorCheckingType Expr (Type SourceAnn)
  | ErrorCheckingKind (Type SourceAnn)
  | ErrorInferringType Expr
  | ErrorInApplication Expr (Type SourceAnn) Expr
  | ErrorInBindingGroup (NonEmpty Ident)
  | ErrorInValueDeclaration Ident
  | ErrorInTypeDeclaration Ident
  | PositionedError (NonEmpty SourceSpan)
  deriving (Show)
