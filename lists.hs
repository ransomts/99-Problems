-- 1 Problem 1
-- (*) Find the last element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'

myLast = head . reverse

-- 2 Problem 2
-- (*) Find the last but one element of a list.

-- (Note that the Lisp transcription of this problem is incorrect.)

-- Example in Haskell:

-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'

myButLast = head . tail . reverse

-- 3 Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.

-- Example:

-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:

-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

--elementAt xs n = xs !! (n - 1)
elementAt xs 1 = head xs
elementAt xs n = elementAt (tail xs) (n - 1)

-- 4 Problem 4
-- (*) Find the number of elements of a list.

-- Example in Haskell:

-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13

myLength [] = 0
myLength xs = (myLength (tail xs)) + 1

myReverse [] = []
myReverse xs =   (myReverse (tail xs)) ++ [head xs]

-- 6 Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

-- Example in Haskell:

-- *Main> isPalindrome [1,2,3]
-- False
-- *Main> isPalindrome "madamimadam"
-- True
-- *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome xs
  | head xs == head (reverse xs) = isPalindrome $ reverse $ tail $ reverse $ tail xs
  | otherwise = False

-- 7 Problem 7
-- (**) Flatten a nested list structure.

-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

-- Example:

-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
flatten [] = []
