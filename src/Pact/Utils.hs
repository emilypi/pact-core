-- |
-- Copyright :  (c) Emily Pillmore 2019-2019
-- License   :  BSD-2-Clause
-- Maintainer:  Emily Pillmore <emily@kadena.io>
-- Stability :  experimental
-- Portability: non-portable
--
-- pact utils
--
module Pact.Utils where

import Data.Text


tshow :: Show a => a -> Text
tshow = pack . show
