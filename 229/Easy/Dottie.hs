module Dottie where

fix :: (Float -> Float) -> Float -> Float -> Float
fix f e a = let a' = f a in if abs (a - a') < e then a else fix f e a'

dottie :: Int -> Float
dottie precision = fix cos (0.1^precision) 0.7
