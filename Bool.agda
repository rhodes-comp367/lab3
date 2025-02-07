module Bool where

data Bool : Set where
  false : Bool
  true : Bool

not : Bool → Bool
not false = true
not true = false

and : Bool → Bool → Bool
and false _ = false
and true x = x

or : Bool → Bool → Bool
or false x = x
or true _ = true

