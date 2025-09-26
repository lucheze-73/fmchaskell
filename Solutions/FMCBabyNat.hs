module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero x = O

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S x) = x
pred zero = zero

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O 
even (S O) = O
even (S (S x)) = even x

odd :: Nat -> Nat
odd O = O 
odd (S O) = S O
odd (S (S x)) = odd x

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus x O = x
monus (S x) (S y) = monus x y

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
(*) _ * O = O
(*) x * (S y) = x * y + x

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) x O = one
(^) x (S y) = x ^ y * x

infixr 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
(/) O x = O
(/) x y = 
  case y of 
    O -> undefined
    S z -> 
      case monus x y of 
        O -> 
          case monus y x of 
            O -> one
            p -> O
        z -> S ((x -* y) / y)     

infixl 7 *

-- remainder
(%) :: Nat -> Nat -> Nat
(%) _ O = undefined
(%) x y = x -* (y * (x / y))

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) O _ = undefined
(|||) (S x) O = S O
(|||) (S x) y =
				case y % S x of
					O -> S O
					z -> O
        				
-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O O = O
absDiff O x = x
absDiff x O = x
absDiff (S x) (S y) = absDiff x y

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = one
factorial one = one
factorial (S x) = factorial x * (S x)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg S x = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined
lo O _ = undefined
lo one x = undefined
lo x one = O
lo x y = S(lo x (y/x))
