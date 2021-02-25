module Answer where

import PGF (Tree)
import Query
import Data.Data.Lens (template)
import Control.Lens (toListOf, over, Traversal')

transfer :: Tree -> Tree
transfer = gf . answer . fg

transfer2 :: Tree -> Tree
transfer2 = gf . expandNatQuestion . fg
-- transfer2 = gf . iden . fg

iden :: GQuestion -> GQuestion
iden gq = gq

-- transfer :: Mode -> Tree -> Tree
-- transfer m = gf . trans . fg where
--   trans = case m of
--     MNone -> id
--     MAns  -> answer
--     -- MMinimalize -> minimalize
--     -- MMaximize   -> maximize
--

expandNatQuestion :: GQuestion -> GQuestion
expandNatQuestion q = over template expandNat q

-- expandNatQuestion q = case q of
--   GIsOdd x -> GIsOdd (expandNatObj x)
--   GIsEven x -> GIsEven (expandNatObj x)
--   GIsPrime x -> GIsPrime (expandNatObj x)

-- expandNatObj :: GObject -> GObject
-- expandNatObj o = over template expandNat o
-- -- expandNatObj o = case o of
-- --   GNatObj x -> GNatObj (expandNat x)
  
expandNat :: GNat -> GNat
expandNat n = case n of
  GListFun f (GListNat xs) -> foldr1 (GBinFun f) (map expandNat xs) --arbitrary, could use foldl1
  GBinFun f x y -> GBinFun f (expandNat x) (expandNat y)
  x -> x

-- compressNat :: GNat -> GNat

-- expandNatList :: GFun2 -> GListNat -> GNat
-- expandNatList f2 ls = 
--   case ls of 
--     GBaseNat n1 n2 -> GBinFun f2 n1 n2
--     -- can't do recursive nesting? (expandNatList n1) (expandNatList 
--     GConsNat n1 ns -> GBinFun f2 n1 (expandNatList f2 ls)

data Mode = MNone | MAns deriving Show -- |  MMaximize | MMinimalize deriving Show

answer :: GQuestion -> GAnswer
answer p = case p of
  GIsOdd x -> test odd x
  GIsEven x -> test even x
  GIsPrime x -> test prime x

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

