-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- AST binder data types
--
module Pact.AST.Binders where




data Binder
  = LiteralBinder SourceSpan
