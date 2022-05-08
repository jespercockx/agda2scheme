
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

not : Bool → Bool
not true = false
not false = true

data D : Set where
  c : @0 Bool → Bool → D

f : D → D
f (c x y) = c (not x) (not y)

data Vec (@0 A : Set) : @0 Nat → Set where
  []  : Vec A zero
  _∷_ : {@0 n : Nat} → A → Vec A n → Vec A (suc n)


map : {@0 A B : Set} {@0 n : Nat} → (A → B) → Vec A n → Vec B n
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

test1 = map f (c true true ∷ (c false false ∷ []))
