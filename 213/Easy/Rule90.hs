module Rule90 where

import Data.Bits (xor)
import Data.List (unfoldr, intercalate)

readBits :: String -> [Bool]
readBits = map readBit
  where readBit '0' = False
        readBit '1' = True
        readBit _ = error "meh"

showBits :: [Bool] -> String
showBits = map showBit
  where showBit True = 'x'
        showBit _ = ' '

data View = View
  { viewLeft :: [Bool]
  , viewCell :: Bool
  , viewRight :: [Bool]
  } deriving Show

intoView :: [Bool] -> Maybe View
intoView [] = Nothing
intoView (c:r) = Just (View [] c r)

moveLeft :: View -> Maybe View
moveLeft (View [] _ _) = Nothing
moveLeft (View (l:ls) c r) = Just (View ls l (c:r))

moveRight :: View -> Maybe View
moveRight (View _ _ []) = Nothing
moveRight (View l c (r:rs)) = Just (View (c:l) r rs)

peekLeft :: View -> Bool
peekLeft = maybe False viewCell . moveLeft

peekRight :: View -> Bool
peekRight = maybe False viewCell . moveRight

type Rule = Bool -> Bool -> Bool -> Bool

rule90 :: Rule
rule90 l _ r = l `xor` r

next :: Rule -> [Bool] -> [Bool]
next rule = unfoldr (fmap f) . intoView
  where f v = let c = rule (peekLeft v) (viewCell v) (peekRight v)
              in (c, moveRight v)

run :: Rule -> [Bool] -> [[Bool]]
run = iterate . next

runAndShow :: Rule -> Int -> [Bool] -> String
runAndShow rule n = intercalate "\n" . map showBits . take n . run rule
