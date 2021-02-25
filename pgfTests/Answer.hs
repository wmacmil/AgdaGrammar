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

-- evalNat :: GNat -> Int
-- evalNat gn = case gn of
--   GPlus  x y -> (evalNat x) + (evalNat y)
--   GTimes x y -> (evalNat x) * (evalNat y)
--   GNumber (GInt i) -> i 
--
  
  -- BinFun : Fun2 -> Nat -> Nat -> Nat ;

  -- GPConjs co (GListProp ps) -> foldl1 (GPConj co) (map minimalizeP ps)
  --
  --   ListFun  : Fun2 -> [Nat] -> Nat ;



evalNat :: GNat -> Int
evalNat gn = case gn of
  GBinFun GPlus  x y -> (evalNat x) + (evalNat y)
  GBinFun GTimes x y -> (evalNat x) * (evalNat y)
  GListFun GPlus (GListNat xs) -> foldl (+) 0 (map evalNat xs)
  GListFun GTimes (GListNat xs) -> foldl (*) 1 (map evalNat xs)
  GNumber (GInt i) -> i 

-- fold the list with the operator

-- evalNat (GNumber (GInt 3))
-- evalNat (GTimes (GNumber (GInt 3)) (GNumber (GInt 3)))

value :: GObject -> Int
value e = case e of
  GNatObj x -> (evalNat x) 

test :: (Int -> Bool) -> GObject -> GAnswer
test f x = if f (value x) then GYes else GNo

-- could optimize so that one could, for instance, regonize if it is the product of two number
prime :: Int -> Bool
prime x = elem x primes where
  primes = sieve [2 .. x]
  sieve (p:xs) = p : sieve [n | n <- xs, mod n p > 0]
  sieve [] = []

