module Garland where

import Data.List

commons :: String -> [String]
commons word = (tail . tails) word `intersect` (init . inits) word

garland :: String -> Int
garland = maximum . map length . commons
