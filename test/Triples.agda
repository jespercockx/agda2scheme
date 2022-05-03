
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

variable A B : Set

data List (A : Set) : Set where
  [] : List A
  _::_ : A → List A → List A

infixr 5 _::_

filter : (A → Bool) → List A → List A
filter p [] = []
filter p (x :: xs) with p x
... | true  = x :: filter p xs
... | false = filter p xs
-- ^ I'm using `with` instead of `if_then_else_` so that this test
--   case also works with strict evaluation.

_++_ : List A → List A → List A
[]        ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

_>>=_ : List A → (A → List B) → List B
[]        >>= f = []
(x :: xs) >>= f = f x ++ (xs >>= f)

range : Nat → Nat → List Nat
range x y = go (suc y - x) x
  where
    go : Nat → Nat → List Nat
    go zero    _ = []
    go (suc m) n = n :: go m (suc n)

record Triple : Set where
  constructor triple
  field
    fst snd trd : Nat

alltriples : Nat → List Triple
alltriples top = range 1 top >>= λ z → range 1 z >>= λ y → range 1 y >>= λ x → (triple x y z) :: []

cartesian : Triple → Bool
cartesian (triple x y z) = x * x + y * y == z * z

triples : Nat → List Triple
triples top = filter cartesian (alltriples top)

sumall : List Triple → Nat
sumall [] = 0
sumall (triple x y z :: xs) = x + y + z + sumall xs

test1 = sumall (triples 200) -- evaluates to 33638
