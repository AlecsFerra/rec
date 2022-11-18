module Semantics.FixPoint (fix) where

import Data.Maybe (mapMaybe)

fix :: a -> (a -> a) -> (a -> Maybe b) -> b
fix bottom f g = head $ mapMaybe g (iterate f bottom)
