import Data.List

-- Exercises 1 and 2: Custom length with declared type

myLength :: [a] -> Int

myLength [] = 0

myLength (x:xs) = 1 + myLength xs

-- Exercise 3: Compute the mean of a list

myMean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Exercise 4: Convert a list into a palindrome

-- Baremetal (Rebuild and append during unraveling of recursion)
makePalindrome :: [a] -> [a]
makePalindrome (x:xs) = if (length xs) == 0
                        then [x] ++ [x]
                        else [x] ++ (makePalindrome xs) ++ [x]

-- Simple
makePal xs = xs ++ (reverse xs)

-- Exercise 5: Test if item is palindrome
palindrome [] = True
palindrome [x] = True
palindrome xs = if (head xs) == (last xs)
                then palindrome (init (tail xs))
                else False

-- Exercise 6: Sort list of lists by length of sublist
sortByLength xs = sortBy sortLength xs

-- Ordering rule
sortLength xs1 xs2
  | length xs1 < length xs2 = LT
  | length xs1 > length xs2 = GT

-- Exercise 7: Join list of lists by seperator value
-- (I dont think they meant for it to work with non-strings)
join sep [x] = x
join sep (x:xs) = x ++ [sep] ++ (join sep xs)

-- Exercise 8: Height of a binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

height (Node _ Empty Empty) = 0
height (Node _ left  Empty) = 1 + height left
height (Node _ Empty right) = 1 + height right
height (Node _ left  right) = 1 + max (height left) (height right)

-- Exercise 9: 2d points A,B,C; calculate the turn from B->C compared to A->B
data Direction a = LeftTurn
                 | RightTurn
                 | NoTurn
                 | ReverseTurn

getPoint (a1,a2) = sqrt (fromIntegral (a1 * a1) + fromIntegral(a2 * a2))

describeDirection angle
  |  angle == 0 = NoTurn
  |  angle  > 0 = LeftTurn
  |  angle  < 0 = RightTurn
 
-- Use unit vector to compare headings
getDirection (a1,a2) (b1,b2) (c1,c2) = describeDirection ((atan2 (getPoint (a1,a2)) (getPoint (b1,b2))) - (atan2 (getPoint (b1,b2)) (getPoint (c1,c2))))
