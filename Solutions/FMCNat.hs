{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )
import System.Win32 (xBUTTON1)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    (S x) == (S y) = x == y
    _ == _ = False

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    (S x) <= (S y) = x <= y
    _ <= _ = False

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min O _ = O
    min _ O = O
    min (S x) (S y) = S (min x y)

    max :: Nat -> Nat -> Nat
    max O x = x
    max x O = x
    max (S x) (S y) = S (max x y)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero x = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S x) = x
pred zero = zero

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S x)) = even x

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S x)) = odd x

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n O = n
(<+>) n (S m) = S (n + m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus x O = x
monus (S x) (S y) = monus x y

(<->) :: Nat -> Nat -> Nat
(<->) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times _ O = O
times x (S y) = x * y + x

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow x O = one
pow x (S y) = times (pow x y) x

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) O x = O
(</>) x y =
  case y of
    O -> undefined
    S z ->
      case monus x y of
        O ->
          case monus y x of
            O -> one
            p -> O
        z -> S ((x - y) </> y)

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = undefined
(<%>) x y = x - (y * (x </> y))

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv _O = undefined


-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) O _ = undefined
(<|>) (S x) O = True
(<|>) (S x) y =
  case y <%> S x of
    O -> True
    z -> False

divides :: Nat -> Nat -> Bool
divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist O _ = O
dist _ O = O
dist (S x) (S y) = dist x y

(|-|) = dist

factorial :: Nat -> Nat
factorial O = one
factorial (S x) = factorial x * S x

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S x) = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined
lo O _ = undefined
lo (S O) _ = undefined
lo x y =
  case y <-> x of
    O ->
      case x <-> y of
        O -> one
        S z -> O
    z -> S (lo x (y </> x))


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat x =
  if x <= 0 
    then undefined 
  else S (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S x) = 1 + fromNat x

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))
