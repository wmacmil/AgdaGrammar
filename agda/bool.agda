module bool where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

-- open import Data.Empty using (⊥)
open import Data.Empty 

data bool : Set where
  false : bool
  true : bool

caseBool : ∀ {ℓ} → {A : Set ℓ} → A → A → bool → A
caseBool f t false = f
caseBool f t true = t

indBool : {A : bool → Set} → A false → A true → (b : bool) → A b
indBool f t false = f
indBool f t true = t

neg : Set → Set
neg A = A → ⊥

falseNeqTrue : neg (false ≡ true)
-- falseNeqTrue () -- easy way out
falseNeqTrue = λ x → subst (caseBool {A = Set} bool ⊥) x false

-- not sure whats happening beyond this
-- lem1 : (y : bool) → true ≡ y → true ≡ y
-- lem1 true p = p
-- lem1 : (y:bool) (p:Path bool true y) -> Path bool true y = split
-- false -> \ (p : Path bool true false) -> p
-- true -> \ (p : Path bool true true) -> <i>true

data nat : Set where
  zero : nat
  suc : nat → nat

one : nat
one = suc zero

pred : nat → nat
pred zero = zero
pred (suc n) = n

add : (m : nat) → nat → nat
add m zero = m
add m (suc n) = suc (add m n)

add0 : (n : nat) → (add zero n) ≡ n
add0 zero = refl
add0 (suc n) = cong suc (add0 n)

equalNat : nat → nat → bool
equalNat zero zero = true
equalNat zero (suc n2) = false
equalNat (suc n1) zero = false
equalNat (suc n1) (suc n2) = equalNat n1 n2

-- equalNat : nat -> nat -> bool = split
-- zero -> split@(nat -> bool) with
-- zero  -> true
-- suc n -> false
-- suc m -> split@(nat -> bool) with
-- zero  -> false
-- suc n -> equalNat m n



-- add : (m : nat) → nat → nat
-- add m zero = m
-- add m (suc n) = add m n

