-- This is a cut-down version of the code at https://jesper.sikanda.be/posts/formalize-all-the-things.html

open import Agda.Primitive
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.Equality

infixr 5 _∷_
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

{-# BUILTIN LIST List #-}

variable
  A B C : Set
  x y z : A
  k l m n : Nat

it : {{x : A}} → A
it {{x}} = x

data Maybe (A : Set) : Set where
  just    : A → Maybe A
  nothing :     Maybe A

mapMaybe : (A → B) → (Maybe A → Maybe B)
mapMaybe f (just x) = just (f x)
mapMaybe f nothing = nothing

testMapMaybe = mapMaybe suc (just 5)

record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open _×_

data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

mapInl : (A → B) → A ⊎ C → B ⊎ C
mapInl f (inl x) = inl (f x)
mapInl f (inr y) = inr y

mapInr : (B → C) → A ⊎ B → A ⊎ C
mapInr f (inl x) = inl x
mapInr f (inr y) = inr (f y)

record ⊤ : Set where
  constructor tt     -- no fields

data ⊥ : Set where   -- no constructor

¬_ : Set → Set
¬ A = A → ⊥

sym : x ≡ y → y ≡ x
sym refl = refl

trans : x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

cong : (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

subst : (P : A → Set) → x ≡ y → P x → P y
subst P refl p = p

module Nat-≤ where

  data _≤_ : Nat → Nat → Set where
    ≤-zero :         zero  ≤ n
    ≤-suc  : m ≤ n → suc m ≤ suc n

  ≤-refl : n ≤ n
  ≤-refl {n = zero}  = ≤-zero
  ≤-refl {n = suc k} = ≤-suc ≤-refl

  ≤-trans : k ≤ l → l ≤ m → k ≤ m
  ≤-trans ≤-zero      l≤m         = ≤-zero
  ≤-trans (≤-suc k≤l) (≤-suc l≤m) =
    ≤-suc (≤-trans k≤l l≤m)

  ≤-antisym : m ≤ n → n ≤ m → m ≡ n
  ≤-antisym ≤-zero      ≤-zero      = refl
  ≤-antisym (≤-suc m≤n) (≤-suc n≤m) =
    cong suc (≤-antisym m≤n n≤m)

  So : Bool → Set
  So false = ⊥
  So true  = ⊤

  instance
    ≤-dec : {p : So (m < suc n)} → m ≤ n
    ≤-dec {m = zero} {n = n} = ≤-zero
    ≤-dec {m = suc m} {n = suc n} {p = p} =
      ≤-suc (≤-dec {p = p})

record Ord (A : Set) : Set₁ where
  field
    _≤_       : A → A → Set
    ≤-refl    : x ≤ x
    ≤-trans   : x ≤ y → y ≤ z → x ≤ z
    ≤-antisym : x ≤ y → y ≤ x → x ≡ y

  _≥_ : A → A → Set
  x ≥ y = y ≤ x

open Ord {{...}}

instance
  Ord-Nat : Ord Nat
  _≤_       {{Ord-Nat}} = Nat-≤._≤_
  ≤-refl    {{Ord-Nat}} = Nat-≤.≤-refl
  ≤-trans   {{Ord-Nat}} = Nat-≤.≤-trans
  ≤-antisym {{Ord-Nat}} = Nat-≤.≤-antisym

instance
  Ord-⊤ : Ord ⊤
  _≤_       {{Ord-⊤}} = λ _ _ → ⊤
  ≤-refl    {{Ord-⊤}} = tt
  ≤-trans   {{Ord-⊤}} = λ _ _ → tt
  ≤-antisym {{Ord-⊤}} = λ _ _ → refl

data Tri {{_ : Ord A}} : A → A → Set where
  less    : {{x≤y : x ≤ y}} → Tri x y
  equal   : {{x≡y : x ≡ y}} → Tri x y
  greater : {{x≥y : x ≥ y}} → Tri x y

record TDO (A : Set) : Set₁ where
  field
    {{Ord-A}} : Ord A               -- superclass Ord
    tri       : (x y : A) → Tri x y

open TDO {{...}} public

triNat : (x y : Nat) → Tri x y
triNat zero zero = equal
triNat zero (suc y) = less
triNat (suc x) zero = greater
triNat (suc x) (suc y) with triNat x y
... | less    {{x≤y}} = less    {{x≤y = Nat-≤.≤-suc x≤y}}
... | equal   {{x≡y}} = equal   {{x≡y = cong suc x≡y}}
... | greater {{x≥y}} = greater {{x≥y = Nat-≤.≤-suc x≥y}}

testTriNat = triNat 3 5

instance
  TDO-Nat : TDO Nat
  Ord-A {{TDO-Nat}} = Ord-Nat
  tri   {{TDO-Nat}} = triNat

data [_]∞ (A : Set) : Set where
  -∞  :     [ A ]∞
  [_] : A → [ A ]∞
  +∞  :     [ A ]∞

variable
  lower upper : [ A ]∞

module Ord-[]∞ {A : Set} {{ A-≤ : Ord A}} where

  data _≤∞_ : [ A ]∞ → [ A ]∞ → Set where
    -∞-≤ :          -∞   ≤∞   y
    []-≤ : x ≤ y → [ x ] ≤∞ [ y ]
    +∞-≤ :           x   ≤∞  +∞

  []∞-refl : x ≤∞ x
  []∞-refl { -∞}   = -∞-≤
  []∞-refl {[ x ]} = []-≤ ≤-refl
  []∞-refl { +∞}   = +∞-≤

  []∞-trans : x ≤∞ y → y ≤∞ z → x ≤∞ z
  []∞-trans -∞-≤       _          = -∞-≤
  []∞-trans ([]-≤ x≤y) ([]-≤ y≤z) = []-≤ (≤-trans x≤y y≤z)
  []∞-trans _          +∞-≤       = +∞-≤

  []∞-antisym : x ≤∞ y → y ≤∞ x → x ≡ y
  []∞-antisym -∞-≤       -∞-≤       = refl
  []∞-antisym ([]-≤ x≤y) ([]-≤ y≤x) = cong [_] (≤-antisym x≤y y≤x)
  []∞-antisym +∞-≤       +∞-≤       = refl

  instance
    Ord-[]∞ : {{_ : Ord A}} → Ord [ A ]∞
    _≤_       {{Ord-[]∞}} = _≤∞_
    ≤-refl    {{Ord-[]∞}} = []∞-refl
    ≤-trans   {{Ord-[]∞}} = []∞-trans
    ≤-antisym {{Ord-[]∞}} = []∞-antisym

open Ord-[]∞ public

module _ {{_ : Ord A}} where

  instance
    -∞-≤-I : {y : [ A ]∞} → -∞ ≤ y
    -∞-≤-I = -∞-≤

    +∞-≤-I : {x : [ A ]∞} → x ≤ +∞
    +∞-≤-I = +∞-≤

    []-≤-I : {x y : A} {{x≤y : x ≤ y}} → [ x ] ≤ [ y ]
    []-≤-I {{x≤y = x≤y}} = []-≤ x≤y

data BST (A : Set) {{_ : Ord A}}
         (lower upper : [ A ]∞)  : Set where

  leaf : {{l≤u : lower ≤ upper}}
       → BST A lower upper

  node : (x : A)
       → BST A lower [ x ]
       → BST A [ x ] upper
       → BST A lower upper

testBST : BST Nat -∞ +∞
testBST = node 3 leaf leaf

module Lookup {{_ : TDO A}} where

  data _∈_ {lower} {upper} (x : A) :
           (t : BST A lower upper) → Set where
    here  : ∀ {t₁ t₂} → x ≡ y  → x ∈ node y t₁ t₂
    left  : ∀ {t₁ t₂} → x ∈ t₁ → x ∈ node y t₁ t₂
    right : ∀ {t₁ t₂} → x ∈ t₂ → x ∈ node y t₁ t₂

  lookup : ∀ {lower} {upper}
         → (x : A) (t : BST A lower upper) → Maybe (x ∈ t)
  lookup x leaf = nothing
  lookup x (node y t₁ t₂) with tri x y
  ... | less    = mapMaybe left (lookup x t₁)
  ... | equal   = just (here it)
  ... | greater = mapMaybe right (lookup x t₂)

module Insert {{_ : TDO A}} where

  insert : (x : A) (t : BST A lower upper)
         → {{l≤x : lower ≤ [ x ]}} {{x≤u : [ x ] ≤ upper}}
         → BST A lower upper
  insert x leaf = node x leaf leaf
  insert x (node y t₁ t₂) with tri x y
  ... | less    = node y (insert x t₁) t₂
  ... | equal   = node y t₁ t₂
  ... | greater = node y t₁ (insert x t₂)

open Lookup
open Insert

testLookup = lookup 3 testBST

testInsert : BST Nat -∞ +∞
testInsert = insert 4 testBST

fromList : {{_ : TDO A}} → List A → BST A -∞ +∞
fromList [] = leaf {{ l≤u = -∞-≤-I }}
fromList (x ∷ xs) = insert x (fromList xs)

testFromList = fromList (1 ∷ 2 ∷ 3 ∷ [])

_++_ : List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

flatten : {{_ : Ord A}} {lower upper : [ A ]∞} → BST A lower upper → List A
flatten leaf = []
flatten (node x l r) = flatten l ++ (x ∷ flatten r)

testFlatten = flatten testInsert

sort : {{TDO A}} → List A → List A
sort xs = flatten (fromList xs)

test1 = sort (5 ∷ 3 ∷ 9 ∷ 1 ∷ 2 ∷ 10 ∷ 7 ∷ 4 ∷ 8 ∷ 6 ∷ [])
