main :: IO()
main = do
  print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))


-- 1 Problem 1
-- (*) Find the last element of a list.
myLast :: [c] -> c
myLast = head . reverse

-- 2 Problem 2
-- (*) Find the last but one element of a list.
myButLast :: [c] -> c
myButLast = head . tail . reverse

-- 3 Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
--elementAt xs n = xs !! (n - 1)
elementAt xs 1 = head xs
elementAt xs n = elementAt (tail xs) (n - 1)

-- 4 Problem 4
-- (*) Find the number of elements of a list.
myLength :: Num a => [t] -> a
myLength [] = 0
myLength xs = (myLength (tail xs)) + 1

-- 5 Problem 5
-- (*) Reverse a list.
myReverse [] = []
myReverse xs =   (myReverse (tail xs)) ++ [head xs]

-- 6 Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome xs
  | head xs == head (reverse xs) = isPalindrome $ reverse $ tail $ reverse $ tail xs
  | otherwise = False

-- 7 Problem 7
-- (**) Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten = reverse . rec []
  where
  rec acc (List []) = acc
  rec acc (Elem x)  = x:acc
  rec acc (List (x:xs)) = rec (rec acc x) (List xs)

-- 8 Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
compress [] = []
compress (x : xs)
  | xs == [] = [x]
  | x == head xs = compress xs
  | otherwise = x : compress xs
  
-- 9 Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack [] = []
pack [x] = [[x]]
pack (x : xs) = if x `elem` (head (pack xs))
                then (x:(head (pack xs))):(tail (pack xs))
                else [x]:(pack xs)

-- 10 Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: Eq t => [t] -> [(Int, t)]
encode xs = map (\x -> (length x, head x)) (pack xs)


-- 11 Problem 11
-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Single a | Multiple Int a
                  deriving (Show)
encodeModified :: Eq a => [a] -> [ListItem a]                           
encodeModified = map encodeHelper . encode
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x

-- list problems go through 30
