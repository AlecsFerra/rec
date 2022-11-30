module Semantics.FixPoint (applyFixpoint) where

import Data.Maybe (mapMaybe)

applyFixpoint :: a -> (a -> a) -> (a -> Maybe b) -> b
-- Take the first (f^n bottom) satisfying g
applyFixpoint bottom f g = head $ mapMaybe g sequence
  where
    -- Construct the function application sequence
    -- f^0 bottom, f^1 bottom, f^2 bottom, ...
    sequence = iterate f bottom
