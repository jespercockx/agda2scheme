
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

variable A B : Set

if_then_else_ : Bool → A → A → A
if true  then x else y = x
if false then x else y = y

data List (A : Set) : Set where
  [] : List A
  _::_ : A → List A → List A

filter : (A → Bool) → List A → List A
filter p [] = []
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs

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

triples : Nat → List Triple
triples top =
  filter (λ (triple x y z) → x * x + y * y == z * z)
         (range 1 top >>= λ z → range 1 z >>= λ y → range 1 y >>= λ x → (triple x y z) :: [])

test1 = triples 100
