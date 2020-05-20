
main :: IO ()
main = putStrLn "Hello, World"

doubleMe x = x + x

quadMe x = x * 4

releaseMe = do
  putStrLn "I don't want to be bread! Release me from this wack-ass bread prison!"

releaseHim x = if x == 0
               then putStrLn "Thank you for releasing me, have this magic melon."
               else releaseMe

testMap xs = map quadMe xs

mymember x [] = False
mymember x xs = if x == head xs
                then True
                else mymember x (tail xs)

palindrome [] = True
palindrome [x] = True
palindrome xs = if (head xs) == (last xs)
                then palindrome (init (tail xs))
                else False

divides d n = rem n d == 0

ld n = ldf 2 n

ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ld n == n

ident 4 = 4
ident 5 = 5
ident n = n*2

parts (x:xs) = xs

--   Type   Data [Components]
data Book = Book String Int
         deriving(Show)

describe :: Book -> (String, Int)
describe (Book name count) = (name, count+1)


third (_,_,c) = c

-- Compiler-created accessors
-- Also allows for informative creation (list name of parameter during construction)
data Customer = Customer {
    customerID      :: Int
  , customerName    :: String
  , customerAddress :: String
  } deriving (Show)

-- Generics
-- data Maybe a = Just a
--             | Nothing

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

letTest (x:xs) = let i = 4
                 in x + i

whereTest (x:xs) = x + i
  where i = 4

