{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "Nil List"
head (x : xs) = x

tail :: [a] -> [a]
tail [] = error "Nil List"
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null xs = False

length :: Integral i => [a] -> i
length [] = 0
length (x : xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ [] = []
[] ++ xs = xs
(x : xs) ++ ys = x: (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = error "Nil list"
minimum (x : xs) =
  case xs of
    [] -> x
    (y : ys) ->
      if x <= y
        then minimum (x : ys)
        else minimum (y : ys)

maximum :: Ord a => [a] -> a
maximum [] = error "Nil list"
maximum (x : xs) =
  case xs of
    [] -> x
    (y : ys) ->
      if y <= x
        then maximum (x : ys)
        else maximum (y : ys)

take :: Int -> [a] -> [a]
take i [] = error "Not enough items in list"
take 1 (x : xs) = [x]
take i (x : xs) = x : take (i-1) xs

drop :: Int -> [a] -> [a]
drop i [] = []
drop 0 xs = xs
drop i (x : xs) = drop (i - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile z [] = []
takeWhile z (x : xs) =
  if z x
    then x: takeWhile z xs
    else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile z [] = []
dropWhile z (x : xs) =
  if z x
    then dropWhile z xs
    else x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = error "Nil List"
init [x] = []
init (x : xs) = x: init xs

inits :: [a] -> [[a]]
inits [] = error "Nil List"
inits [x] = [[]]
inits xs = snoc (init xs) (inits (init xs))

-- subsequences
any :: (t -> Bool) -> [t] -> Bool
any z [] = False
any z [x] = z x
any z (x : xs) = z x || any z xs

all :: (t -> Bool) -> [t] -> Bool
all z [] = error "Nil List"
all z [x] = z x
all z (x : xs) = z x && all z xs

and :: [Bool] -> Bool
and [] = True
and [z] = z
and (z : zs) = z && and zs

or :: [Bool] -> Bool
or [] = False
or [z] = z
or (z : zs) = z || or zs

concat :: [[a]] -> [a]
concat [] = []
concat [[], xs] = xs
concat (xs : xss) = xs ++ concat xss

-- elem using the funciton 'any' above
elem :: Eq m => m -> [m] -> Bool
elem x [] = False
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x : xs) = (e == x) || elem' e xs

(!!) :: Int -> [a] -> a
_ !! [] = error "Not enough items in list"
0 !! (x : xs) = x
i !! (x : xs) = (i - 1) !! xs

filter :: (a -> Bool) -> [a] -> [a]
filter z [] = []
filter z (x : xs) =
  if z x
    then x : filter z xs
    else filter z xs

map :: (m -> n) -> [m] -> [n]
map z [] = []
map z (x : xs) = z x : map z xs

cycle :: [a] -> [a]
cycle [] = []
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = [x] ++ repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate i x = [x] ++ replicate (i - 1) x

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf (x : xs) [] = False
isPrefixOf (x : xs) (y : ys)= (x == y) && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf (m : ns) [] = False
isInfixOf (m : ns) (i : js) = 
  if isSuffixOf (m : ns) (i : js)
    then True
    else
      if (m == i)
        then isPrefixOf ns js
        else isInfixOf (m : ns) js

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] [] = True
isSuffixOf xs [] = False
isSuffixOf (x : xs) (y : ys) =
  if length ys > length xs
    then isSuffixOf (x : xs) ys
    else (x == y) && isSuffixOf xs ys

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip (x : xs) [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z [] _ = []
zipWith z _ [] = []
zipWith z (x : xs) (y : ys) = z x y : zipWith z xs ys

intercalate :: a -> [a] -> [a]
intercalate _ [] = []
intercalate _ [x] = [x]
intercalate x (y : ys) = y :(x : intercalate x ys)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (/= x) xs)

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)
      -- it crashes if n is greater then the length of xs
splitAt :: Int -> [a] -> ([a], [a])
splitAt i [] =
  case i of
    0 -> ([],[])
    y -> error "Nil lst"
splitAt i xs
  | i == length xs = (xs,[])
  | i == 0 = ([],xs)
  | otherwise = (take i xs, drop i xs)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break z [] = ([],[])
break z xs = (takeWhile (not . z) xs, dropWhile (not . z) xs)

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}