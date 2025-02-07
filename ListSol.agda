module ListSol where

open import Bool
open import Nat

data List (A : Set) : Set where
  nil : List A
  cons : A → List A → List A

-- get the length of a list.
length : {A : Set} → List A → Nat
length nil = zero
length (cons _ xs) = suc (length xs)

-- apply a function to each element of a list.
map : {A B : Set} → (A → B) → List A → List B
map f nil = nil
map f (cons x xs) = cons (f x) (map f xs)

-- `map` doesn't change the length of a list.
length-map : {A B : Set} → (f : A → B) → (xs : List A)
  → Nat= (length (map f xs)) (length xs)
length-map f nil = zero=
length-map f (cons _ xs) = suc= (length-map f xs)

-- get the sum of a list of naturals.
sum : List Nat → Nat
sum nil = zero
sum (cons n ns) = plus n (sum ns)

-- determine whether all elements are true; return true if nil.
all : List Bool → Bool
all nil = true
all (cons x xs) = and x (all xs)

-- append an element to the end of a list.
snoc : {A : Set} → List A → A → List A
snoc nil x = cons x nil
snoc (cons x xs) y = cons x (snoc xs y)

-- `snoc` increases the length of a list by one.
length-snoc : {A : Set} → (xs : List A) → (x : A)
  → Nat= (length (snoc xs x)) (suc (length xs))
length-snoc nil _ = suc= zero=
length-snoc (cons _ xs) x = suc= (length-snoc xs x)

-- concatenate two lists.
concat : {A : Set} → List A → List A → List A
concat nil ys = ys
concat (cons x xs) ys = cons x (concat xs ys)

-- the length of a concatenated list is the sum of the lengths of the lists.
length-concat : {A : Set} → (xs ys : List A)
  → Nat= (length (concat xs ys)) (plus (length xs) (length ys))
length-concat nil ys = refl (length ys)
length-concat (cons _ xs) ys = suc= (length-concat xs ys)

-- reverse a list.
reverse : {A : Set} → List A → List A
reverse nil = nil
reverse (cons x xs) = snoc (reverse xs) x

