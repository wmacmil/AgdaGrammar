
{-# OPTIONS --omega-in-omega --type-in-type #-}

module ContrClean where

open import Agda.Builtin.Sigma public

variable
  A B : Set

data _≡_ {A : Set} (a : A) : A → Set where
  r : a ≡ a

infix 20 _≡_

id : A → A
id = λ z → z

iscontr : (A : Set) → Set
iscontr A =  Σ A λ a → (x : A) → (a ≡ x)

-- -- Definition
-- (A : Type)=> Contractible A := exist ((a : A))((x : A)-> Id (a)(x)); ; CenterContraction ;

fiber : (A B : Set) (f : A -> B) (y : B) → Set
fiber A B f y = Σ A (λ x → y ≡ f x) -- (x : A) * Path B y (f x)

isEquiv : (A B : Set) → (f : A → B) → Set
isEquiv A B f = (y : B) → iscontr (fiber A B f y)

isEquiv' : (A B : Set) → (f : A → B) → Set
isEquiv' A B f = (y : B) → iscontr (fiber' f y)
  where
    fiber' : (f : A -> B) (y : B) → Set
    fiber' f y = Σ A (λ x → y ≡ f x) -- (x : A) * Path B y (f x)

-- Equivalence ( f : A -> B ) : Set = ( y : B ) -> ( isContr ( fiber it ) ) ; ; ; fiber it : Set = ( x : A ) ( * ) ( Id ( f ( x ) ) ( y ) )


singl : (A : Set) (a : A) → Set
singl A a = Σ A (λ x → a ≡ x) -- = (x : A) * Path A a x

contrSingl : (A : Set) (a b : A) (p : a ≡ b) →  _≡_ {singl A a} (a , r ) (b , p)
contrSingl A a .a r = r

-- -- proof from cubical notes
idIsEquiv : (A : Set) → isEquiv A A (id {A})
idIsEquiv A y = (y , r) , λ x → contrSingl A y (fst x) (snd x)
-- idIsEquiv A y = (y , r) , λ x → let fs = fst x
--                                     sn = snd x
--                                     in contrSingl A y fs sn --contrSingl A y (fst x) (snd x)

-- proof from Aarne
idIsEquiv' : (A : Set) → isEquiv A A (id {A})
idIsEquiv' A y = ybar , λ { (a , r) → r}
  where
    fib' : Set
    fib' = fiber A A id y
    fib : Set
    fib = Σ A (λ x → y ≡ x)
    ybar : fib'
    ybar = y , r

-- before pattern matching b : y ≡ id a
equiv : ( a b : Set ) → Set
equiv a b = Σ (a → b) λ f → isEquiv a b f -- (f : A -> B) * isEquiv A B f

-- perhaps misformulated
eqToIso : ( a b : Set ) → _≡_ {Set} a b → equiv a b
-- eqToIso a .a r = id , idIsEquiv' a
eqToIso a .a r = let lem1 = idIsEquiv' a in (λ z → z) , lem1


-- it breaks down --

-- equiv' : (u : Set₁) ( a b : u ) → Set
-- equiv' u a b = Σ (a → b) λ f → isEquiv a b f -- (f : A -> B) * isEquiv A B f

-- eqToIso' : (u : Set) ( a b : u ) → _≡_ a b → equiv' u {!!} {!!}
-- eqToIso' u a b = {!!}

mymap : (a b : Set) → Set
mymap a b = _≡_ {Set} a b → equiv a b


Id→Eq : (X Y : Set ) → X ≡ Y → equiv X Y
Id→Eq X .X r = (λ z → z) , idIsEquiv X

-- is-univalent : (u : Set) → u -- ⁺ ̇
-- is-univalent u = (X Y : u ) → isEquiv ? ? (Id→Eq X Y)

-- is-univalent : (𝓤 : Universe) → 𝓤 ⁺ ̇
-- is-univalent 𝓤 = (X Y : 𝓤 ̇ ) → is-equiv (Id→Eq X Y)

-- setEquiv : Set
-- setEquiv = (a b : Set) → isEquiv ? ? ((_≡_ a b) → (equiv a b))

-- univU : Set → Set
-- univU u = (a b : u) → isEquiv ? ? (_≡_ a b → equiv a b) -- {!_≡_ {Set} a b → equiv a b!} 

-- what is this unreachable clause stuff?
-- -- proof from cubical notes
-- idIsEquiv : (A : Set) → isEquiv A A (id {A})
-- idIsEquiv A y = (y , r) , λ x → contrSingl A y (fst x) (snd x)
-- idIsEquiv A y = (y , r) , λ x → let fs = fst x ; sn = snd x in contrSingl A y fs sn --contrSingl A y (fst x) (snd x)

