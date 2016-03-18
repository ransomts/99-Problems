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
  
