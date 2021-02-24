module Answer where

import PGF (Tree)
import Query

transfer :: Tree -> Tree
transfer = gf . answer . fg

answer :: GQuestion -> GAnswer
answer p = case p of
  GIsOdd x -> test odd x
  GIsEven x -> test even x
  GIsPrime x -> test prime x

evalNat :: GNat -> Int
evalNat gn = case gn of
  GPlus  x y -> (evalNat x) + (evalNat y)
  GTimes x y -> (evalNat x) * (evalNat y)
  GNumber (GInt i) -> i 

-- evalNat (GNumber (GInt 3))
-- evalNat (GTimes (GNumber (GInt 3)) (GNumber (GInt 3)))

-- so value should just pipe in evalNat to get a Normal Form?
value :: GObject -> Int
value e = case e of
  GNatObj x -> (evalNat x) 

test :: (Int -> Bool) -> GObject -> GAnswer
test f x = if f (value x) then GYes else GNo

prime :: Int -> Bool
prime x = elem x primes where
  primes = sieve [2 .. x]
  sieve (p:xs) = p : sieve [n | n <- xs, mod n p > 0]
  sieve [] = []

