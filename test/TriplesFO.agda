
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

variable A B : Set

data List (A : Set) : Set where
  [] : List A
  _::_ : A → List A → List A

infixr 5 _::_

if_then_else_ : Bool → A → A → A
if true  then x else y = x
if false then x else y = y

id : A → A
id x = x

_++_ : List A → List A → List A
[]        ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

range : Nat → Nat → List Nat
range x y = go (suc y - x) x
  where
    go : Nat → Nat → List Nat
    go zero    _ = []
    go (suc m) n = n :: go m (suc n)

record Triple : Set where
  constructor triple
  field fst snd trd : Nat

bind1 : Nat → Nat → List Nat → List Triple
bind1 y z [] = []
bind1 y z (x :: xs) = triple x y z :: bind1 y z xs

bind2 : Nat → List Nat → List Triple
bind2 z [] = []
bind2 z (y :: ys) = bind1 y z (range 1 y) ++ bind2 z ys

bind3 : List Nat → List Triple
bind3 [] = []
bind3 (z :: zs) = bind2 z (range 1 z) ++ bind3 zs

alltriples : Nat → List Triple
alltriples top = bind3 (range 1 top)

pythagorean : Triple → Bool
pythagorean (triple x y z) = x * x + y * y == z * z


filterP : List Triple → List Triple
filterP [] = []
filterP (x :: xs) = (if pythagorean x then (x ::_) else id) (filterP xs)


triples : Nat → List Triple
triples top = filterP (alltriples top)

sumall : List Triple → Nat
sumall [] = 0
sumall (triple x y z :: xs) = x + y + z + sumall xs

test1 = sumall (triples 200) -- evaluates to 33638
