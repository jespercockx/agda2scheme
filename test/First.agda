postulate A : Set

id : A → A
id x = x

data Bool : Set where
  true false : Bool

not : Bool → Bool
not true  = false
not false = true

ite : {A : Set} → Bool → A → A → A
ite true x y = x
ite false x y = y

{-# NON_TERMINATING #-}
loop : Bool
loop = loop

test1 = ite false loop true

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

one = suc zero
two = suc one
three = suc two

pred : Nat → Nat
pred zero = zero
pred (suc n) = n


plus : Nat → Nat → Nat
plus zero n = n
plus (suc m) n = suc (plus m n)

twice : Nat → Nat
twice zero = zero
twice (suc n) = suc (suc (twice n))

pow2 : Nat → Nat
pow2 zero = suc zero
pow2 (suc n) = twice (pow2 n)

consume : Nat → Nat
consume zero = zero
consume (suc n) = consume n

test2 = consume (pow2 (twice (twice (twice three))))


data Vec (@0 A : Set) : @0 Nat → Set where
  nil : Vec A zero
  con : {n : Nat} → A → Vec A n → Vec A (suc n)

head : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → A
head (con x xs) = x

tail : {@0 A : Set} {@0 n : Nat} → Vec A (suc n) → Vec A n
tail (con x xs) = xs

map : {@0 A B : Set} {@0 n : Nat} → (A → B) → Vec A n → Vec B n
map f nil = nil
map f (con x xs) = con (f x) (map f xs)

test3 = head (tail (map suc (con zero (con (suc zero) (con (suc (suc zero)) nil)))))
