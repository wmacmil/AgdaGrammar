{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Answer where

import qualified PGF (Tree, showExpr)
-- import PGF (Expr)
import PGF hiding (Tree, showExpr)
import Query

-- transfer :: Expr -> Expr
transfer :: PGF.Tree -> PGF.Tree
transfer = gf . answer . fg

transfer2 :: PGF.Tree -> PGF.Tree
-- transfer2 = gf . iden . fg
transfer2 = gf . vAnswer . fg

transfer3 :: PGF.Tree -> PGF.Tree
-- transfer3 = gf . iden . fg
transfer3 = gf . cAnswer . fg

-- for testing
iden :: GQuestion -> GQuestion
iden gq = gq

-- expandNat :: GNat -> GNat
-- the composOp no longer works with this restricted type sig
expandNat :: Tree a -> Tree a
expandNat n = case n of
  GLstFun f (GListNat xs) -> foldr1 (GBinFun f) (map expandNat xs)
  GBinFun f x y -> GBinFun f (expandNat x) (expandNat y)
  GIsNumProp (GLstNumProp c (GListNumPred xs)) n
    -> foldr1 (GPConj c) (map (\x -> GIsNumProp x (expandNat n)) (map expandNat xs)) -- do really need expandNats everywhere
  GIsNumProp x n -> GIsNumProp x (expandNat n)
  x -> composOp expandNat x

-- is it the case that 3 is prime , odd and even
--   Simple:      no .
--   Verbose:     no . it is not the case that 3 is prime , odd and even .

-- Query> p -cat=Prop "3 is even or odd"
-- IsNumProp (LstNumProp Or (BaseNumPred Even Odd)) (NatObj (Number 3))
-- Query> p -cat=Prop "3 is even or 3 is odd"
-- PConj Or (IsNumProp Even (NatObj (Number 3))) (IsNumProp Odd (NatObj (Number 3)))

-- x is even or odd
  
-- -- assumes everything inside is binary conjunctions
-- -- >>> mergeCong 
-- mergeConj :: GConj -> GProp -> GProp -> GListProp
-- mergeConj co p q = GListProp (getConj p ++ getConj q)
--  where
--   getConj :: GProp -> [GProp]
--   getConj p = case p of
--     GPConj ko p1 p2 | ko == co -> getConj p1 ++ getConj p2
--     _ -> [p]

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

cAnswer :: GQuestion -> GAnswer
cAnswer q = case q of
  GPropQuest prop -> cTestProp (evalProp prop) prop

cTestProp :: Bool -> GProp -> GAnswer
cTestProp b prop =
  if b == True
  then GYesProp (composOp compressNat prop)
  else GNoProp (composOp compressNat prop)

--v as in verbose
vAnswer :: GQuestion -> GAnswer
vAnswer q = case q of
  GPropQuest prop -> vTestProp (evalProp prop) prop

vTestProp :: Bool -> GProp -> GAnswer
vTestProp b prop =
  if b == True
  then GYesProp (composOp expandNat prop)
  else GNoProp (composOp expandNat prop)

test :: (Int -> Bool) -> GObject -> GAnswer
test f x = if f (value x) then GYes else GNo

answer :: GQuestion -> GAnswer
answer p = case p of
  GPropQuest prop -> testProp (evalProp prop)

testProp :: Bool -> GAnswer
testProp b = if b == True then GYes else GNo

testB :: (Int -> Bool) -> GObject -> Bool
testB f x = f (value x)

evalProp :: GProp -> Bool
evalProp p = case p of
  GIsNumProp GOdd obj -> testB odd obj
  GIsNumProp GEven obj -> testB even obj
  GIsNumProp GPrime obj -> testB prime obj
  GIsNumProp (GLstNumProp c (GListNumPred [])) obj -> 
    case c of
      GAnd -> True
      GOr -> False
  -- GIsNumProp (GLstNumProp c (GListNumPred [x])) obj -> evalProp (GIsNumProp x obj)
  -- GIsNumProp (GLstNumProp c (GListNumPred (x : x' : xs))) obj -> 
  GIsNumProp (GLstNumProp c (GListNumPred (x : xs))) obj -> 
    let xo = evalProp (GIsNumProp x obj) 
        xso = evalProp (GIsNumProp (GLstNumProp c (GListNumPred (xs))) obj) in
        -- x'o = evalProp (GIsNumProp x' obj) in
    case c of
      GAnd -> (&&) xo xso -- (testB _ _) _
      GOr -> (||) xo xso
  GIf p1 p2 -> not (evalProp p1) || (evalProp p2)
  GNot p -> not (evalProp p)
  GPConj c p1 p2 ->
    case c of
      GAnd -> (evalProp p1) && (evalProp p2)
      GOr -> (evalProp p1) || (evalProp p2)

  -- IsNumProp : NumPred -> Object -> Prop ;
  -- LstNumProp : Conj -> [NumPred] -> NumPred ;

value :: GObject -> Int
value e = case e of
  GNatObj x -> (evalNat x)

evalNat :: GNat -> Int
evalNat gn = case gn of
  GBinFun GPlus  x y -> (evalNat x) + (evalNat y)
  GBinFun GTimes x y -> (evalNat x) * (evalNat y)
  GLstFun GPlus (GListNat xs) -> foldl (+) 0 (map evalNat xs)
  GLstFun GTimes (GListNat xs) -> foldl (*) 1 (map evalNat xs)
  GNumber (GInt i) -> i

-- could optimize so that one could, for instance, regonize if it is the product of two number
prime :: Int -> Bool
prime x = elem x primes where
  primes = sieve [2 .. x]
  sieve (p:xs) = p : sieve [n | n <- xs, mod n p > 0]
  sieve [] = []

--------------------------testing----------------------------------

--for propositional extension
-- >>> three = (GNumber (GInt 3))
-- >>> prime3 = (GIsNumProp GEven (GNatObj three))
-- >>> evalProp prime3
-- False
-- >>> gf $ testProp (evalProp prime3 )
-- EFun No
-- >>> prime3q = GPropQuest prime3
-- >>> answer3even = answer prime3q
-- >>> gf $ prime3q
-- EApp (EFun PropQuest) (EApp (EApp (EFun IsNumProp) (EFun Even)) (EApp (EFun NatObj) (EApp (EFun Number) (ELit (LInt 3)))))
-- >>> gf $ answer3even
-- EFun No
-- >>> gr <- readPGF "Query.pgf"
-- >>> eng = head $ languages gr
-- >>> linearize gr eng $ gf $ prime3q
-- "is it the case that 3 is even"

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

-- stack test --file-watch
-- cabal test suite

----Stuff that didn't work----

-- data AnswerType = Simple | Verbose | Compressed
--   deriving (Eq, Show)
-- responseType :: AnswerType -> GAnswer
-- responseType Simple     = _
-- responseType Verbose    = _
-- responseType Compressed = _
-- -- (composOp compressNat obj)
-- test' :: AnswerType -> (Int -> Bool) -> GObject -> GAnswer
-- test' Simple f x = g f x GYes GNo -- answerQ f x GYes GNo -- if f (value x) then GYes else GNo
-- test' Verbose f x = g f x  -- if f (value x) then GYes else GNo
-- test' Compressed f x = g f x --if f (value x) then GYes else GNo
--   where
--     answerQ f x yes no = if f (value x) then yes else no
---- data AnswerType = Verbose | Compressed
----   deriving (Eq, Show)
--
---- cvAnswer :: AnswerType -> GQuestion -> GAnswer
---- cvAnswer cOrv q = case q of
----   GIsNumPred GOdd x -> cvTest cOrv GOdd odd x
----   GIsNumPred GEven x -> cvTest cOrv GEven even x
----   GIsNumPred GPrime x -> cvTest cOrv GPrime prime x
--
---- cvTest :: AnswerType -> GNumPred -> (Int -> Bool) -> GObject -> GAnswer
---- cvTest Verbose p f obj = helper expandNat p f obj
---- cvTest Compressed p f obj = helper compressNat p f obj
--
---- -- why is the type inference wrong here
---- helper :: forall a.  (Tree a -> Tree a) -> GNumPred -> (Int -> Bool) -> GObject -> GAnswer
---- helper cv p f obj =
----   if f (value obj)
----   then GYesIsNumPred p (composOp cv obj)
----   else GNoIsNumPred p (composOp cv obj)
