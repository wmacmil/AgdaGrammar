{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Query where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GAnswer = Tree GAnswer_
data GAnswer_
type GFun2 = Tree GFun2_
data GFun2_
type GListNat = Tree GListNat_
data GListNat_
type GNat = Tree GNat_
data GNat_
type GObject = Tree GObject_
data GObject_
type GQuestion = Tree GQuestion_
data GQuestion_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GNo :: Tree GAnswer_
  GYes :: Tree GAnswer_
  GPlus :: Tree GFun2_
  GTimes :: Tree GFun2_
  GListNat :: [GNat] -> Tree GListNat_
  GBinFun :: GFun2 -> GNat -> GNat -> Tree GNat_
  GListFun :: GFun2 -> GListNat -> Tree GNat_
  GNumber :: GInt -> Tree GNat_
  GNatObj :: GNat -> Tree GObject_
  GIsEven :: GObject -> Tree GQuestion_
  GIsOdd :: GObject -> Tree GQuestion_
  GIsPrime :: GObject -> Tree GQuestion_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GNo,GNo) -> and [ ]
    (GYes,GYes) -> and [ ]
    (GPlus,GPlus) -> and [ ]
    (GTimes,GTimes) -> and [ ]
    (GListNat x1,GListNat y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GBinFun x1 x2 x3,GBinFun y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GListFun x1 x2,GListFun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNumber x1,GNumber y1) -> and [ x1 == y1 ]
    (GNatObj x1,GNatObj y1) -> and [ x1 == y1 ]
    (GIsEven x1,GIsEven y1) -> and [ x1 == y1 ]
    (GIsOdd x1,GIsOdd y1) -> and [ x1 == y1 ]
    (GIsPrime x1,GIsPrime y1) -> and [ x1 == y1 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAnswer where
  gf GNo = mkApp (mkCId "No") []
  gf GYes = mkApp (mkCId "Yes") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "No" -> GNo 
      Just (i,[]) | i == mkCId "Yes" -> GYes 


      _ -> error ("no Answer " ++ show t)

instance Gf GFun2 where
  gf GPlus = mkApp (mkCId "Plus") []
  gf GTimes = mkApp (mkCId "Times") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Plus" -> GPlus 
      Just (i,[]) | i == mkCId "Times" -> GTimes 


      _ -> error ("no Fun2 " ++ show t)

instance Gf GListNat where
  gf (GListNat [x1,x2]) = mkApp (mkCId "BaseNat") [gf x1, gf x2]
  gf (GListNat (x:xs)) = mkApp (mkCId "ConsNat") [gf x, gf (GListNat xs)]
  fg t =
    GListNat (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseNat" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsNat" -> fg x1 : fgs x2


      _ -> error ("no ListNat " ++ show t)

instance Gf GNat where
  gf (GBinFun x1 x2 x3) = mkApp (mkCId "BinFun") [gf x1, gf x2, gf x3]
  gf (GListFun x1 x2) = mkApp (mkCId "ListFun") [gf x1, gf x2]
  gf (GNumber x1) = mkApp (mkCId "Number") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "BinFun" -> GBinFun (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "ListFun" -> GListFun (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Number" -> GNumber (fg x1)


      _ -> error ("no Nat " ++ show t)

instance Gf GObject where
  gf (GNatObj x1) = mkApp (mkCId "NatObj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NatObj" -> GNatObj (fg x1)


      _ -> error ("no Object " ++ show t)

instance Gf GQuestion where
  gf (GIsEven x1) = mkApp (mkCId "IsEven") [gf x1]
  gf (GIsOdd x1) = mkApp (mkCId "IsOdd") [gf x1]
  gf (GIsPrime x1) = mkApp (mkCId "IsPrime") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "IsEven" -> GIsEven (fg x1)
      Just (i,[x1]) | i == mkCId "IsOdd" -> GIsOdd (fg x1)
      Just (i,[x1]) | i == mkCId "IsPrime" -> GIsPrime (fg x1)


      _ -> error ("no Question " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GBinFun x1 x2 x3 -> r GBinFun `a` f x1 `a` f x2 `a` f x3
    GListFun x1 x2 -> r GListFun `a` foldr (a . a (r (:)) . f) (r []) x1 `a` foldr (a . a (r (:)) . f) (r []) x2
    GNumber x1 -> r GNumber `a` f x1
    GNatObj x1 -> r GNatObj `a` f x1
    GIsEven x1 -> r GIsEven `a` f x1
    GIsOdd x1 -> r GIsOdd `a` f x1
    GIsPrime x1 -> r GIsPrime `a` f x1
    GListNat x1 -> r GListNat `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
