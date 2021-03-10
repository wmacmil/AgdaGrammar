{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Answer where

import qualified PGF (Tree, showExpr)
-- import PGF (Expr)
import PGF hiding (Tree, showExpr)
import Query

-- transfer :: Expr -> Expr
transfer :: PGF.Tree -> PGF.Tree
transfer = gf . answer . fg

-- for testing
iden :: GQuestion -> GQuestion
iden gq = gq

transfer2 :: PGF.Tree -> PGF.Tree
transfer2 = gf . expandNatQuestion . fg

transfer3 :: PGF.Tree -> PGF.Tree
transfer3 = gf . compressNatQuestion . fg

-- stack test --file-watch
-- cabal test suite

expandNatQuestion :: GQuestion -> GQuestion
expandNatQuestion = composOp (expandNat)

-- expandNat :: GNat -> GNat -- the composOp no longer works with this restricted type sig
expandNat :: Tree a -> Tree a
expandNat n = case n of
  GLstFun f (GListNat xs) -> foldr1 (GBinFun f) (map expandNat xs)
  GBinFun f x y -> GBinFun f (expandNat x) (expandNat y)
  x -> composOp expandNat x

compressNatQuestion :: GQuestion -> GQuestion
compressNatQuestion = composOp (compressNat)

compressNat :: Tree a -> Tree a
compressNat n = case n of
  GBinFun f x y -> aggregate f $ compressNat (mergeFun f x y)
  x -> composOp compressNat x

mergeFun :: GFun2 -> GNat -> GNat -> GListNat
mergeFun f1 n1 n2 = GListNat (getF n1 ++ getF n2)
  where
    getF :: GNat -> [GNat]
    getF n = case n of
      GBinFun f2 n1 n2 | f1 == f2 -> getF n1 ++ getF n2
      _ -> [n]

aggregate :: GFun2 -> GListNat -> GNat
aggregate f l = GLstFun f l


answer :: GQuestion -> GAnswer
answer p = case p of
  GIsNumPred GOdd x -> test odd x
  GIsNumPred GEven x -> test even x
  GIsNumPred GPrime x -> test prime x

evalNat :: GNat -> Int
evalNat gn = case gn of
  GBinFun GPlus  x y -> (evalNat x) + (evalNat y)
  GBinFun GTimes x y -> (evalNat x) * (evalNat y)
  GLstFun GPlus (GListNat xs) -> foldl (+) 0 (map evalNat xs)
  GLstFun GTimes (GListNat xs) -> foldl (*) 1 (map evalNat xs)
  GNumber (GInt i) -> i

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

--------------------------testing----------------------------------

  -- merging
-- >>> gr <- readPGF "Query.pgf"
-- >>> eng = head $ languages gr
-- >>> n1 = (GBinFun GTimes (GNumber (GInt 3)) (GNumber (GInt 4)))
-- >>> n2 = (GBinFun GTimes (GNumber (GInt 1000)) (GNumber (GInt 2000)))
-- >>> n3 = (GBinFun GPlus (GNumber (GInt 3)) (GNumber (GInt 4)))
-- >>> merged = mergeFun GTimes n1 n3--(GNumber (GInt 5))
-- >>> mergedl = GLstFun GTimes merged
-- >>> linearize gr eng $ gf $ mergedl
-- "the product of 3 , 4 and the sum of 3 and 4"

  -- expanding
-- >>> gr <- readPGF "Query.pgf"
-- >>> cat = startCat gr
-- >>> sum345 = "is the sum of 3 , 4 and 5 prime"
-- >>> treeS345 = head $ head $ parseAll gr cat sum345
-- >>> eng = head $ languages gr
-- >>> foo = fg $ treeS345 :: GQuestion
-- >>> bar' = gf $ expandNat foo
-- >>> linearize gr eng bar'
-- "is the sum of 3 and the sum of 4 and 5 prime"
--
--
-- >>> treeS345
-- >>> PGF.showExpr [] treeS345

  -- evaluating
-- >>> evalNat (GNumber (GInt 3))
-- 3
-- >>> n1 = (GBinFun GTimes (GNumber (GInt 3)) (GNumber (GInt 3)))
-- >>> o1 = GNatObj n1
-- >>> q1 =  GIsNumPred GPrime o1
-- >>> gf $ answer q1
-- EFun No
-- >>> evalNat n1
-- 9

  --general playing with list
-- >>> gr <- readPGF "Query.pgf"
-- >>> cat = startCat gr
-- >>> bn2 = (GListNat [(GNumber (GInt 4)),(GNumber (GInt 5))])
-- >>> bn3 = (GListNat [(GNumber (GInt 3)),(GNumber (GInt 4)),(GNumber (GInt 5))])
-- >>> expanded = gf $ expandNat (GLstFun GPlus bn3)
-- >>> expanded
-- EApp (EApp (EApp (EFun BinFun) (EFun Plus)) (EApp (EFun Number) (ELit (LInt 3)))) (EApp (EApp (EApp (EFun BinFun) (EFun Plus)) (EApp (EFun Number) (ELit (LInt 4)))) (EApp (EFun Number) (ELit (LInt 5))))
-- >>> :i gr
-- gr :: PGF 	-- Defined at <interactive>:2224:2
-- >>>  eng = head $ languages gr
-- >>> linearize gr eng expanded
-- "the sum of 3 and the sum of 4 and 5"
-- >>> linearize gr eng $ gf $ (GLstFun GPlus bn3)
-- "the sum of 3 , 4 and 5"
