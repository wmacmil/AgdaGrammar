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
type GNumPred = Tree GNumPred_
data GNumPred_
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
  GNoIsNumPred :: GNumPred -> GObject -> Tree GAnswer_
  GYes :: Tree GAnswer_
  GYesIsNumPred :: GNumPred -> GObject -> Tree GAnswer_
  GPlus :: Tree GFun2_
  GTimes :: Tree GFun2_
  GListNat :: [GNat] -> Tree GListNat_
  GBinFun :: GFun2 -> GNat -> GNat -> Tree GNat_
  GLstFun :: GFun2 -> GListNat -> Tree GNat_
  GNumber :: GInt -> Tree GNat_
  GEven :: Tree GNumPred_
  GOdd :: Tree GNumPred_
  GPrime :: Tree GNumPred_
  GNatObj :: GNat -> Tree GObject_
  GIsNumPred :: GNumPred -> GObject -> Tree GQuestion_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GNo,GNo) -> and [ ]
    (GNoIsNumPred x1 x2,GNoIsNumPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GYes,GYes) -> and [ ]
    (GYesIsNumPred x1 x2,GYesIsNumPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GPlus,GPlus) -> and [ ]
    (GTimes,GTimes) -> and [ ]
    (GListNat x1,GListNat y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GBinFun x1 x2 x3,GBinFun y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GLstFun x1 x2,GLstFun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNumber x1,GNumber y1) -> and [ x1 == y1 ]
    (GEven,GEven) -> and [ ]
    (GOdd,GOdd) -> and [ ]
    (GPrime,GPrime) -> and [ ]
    (GNatObj x1,GNatObj y1) -> and [ x1 == y1 ]
    (GIsNumPred x1 x2,GIsNumPred y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAnswer where
  gf GNo = mkApp (mkCId "No") []
  gf (GNoIsNumPred x1 x2) = mkApp (mkCId "NoIsNumPred") [gf x1, gf x2]
  gf GYes = mkApp (mkCId "Yes") []
  gf (GYesIsNumPred x1 x2) = mkApp (mkCId "YesIsNumPred") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "No" -> GNo 
      Just (i,[x1,x2]) | i == mkCId "NoIsNumPred" -> GNoIsNumPred (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Yes" -> GYes 
      Just (i,[x1,x2]) | i == mkCId "YesIsNumPred" -> GYesIsNumPred (fg x1) (fg x2)


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
  gf (GLstFun x1 x2) = mkApp (mkCId "LstFun") [gf x1, gf x2]
  gf (GNumber x1) = mkApp (mkCId "Number") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "BinFun" -> GBinFun (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "LstFun" -> GLstFun (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Number" -> GNumber (fg x1)


      _ -> error ("no Nat " ++ show t)

instance Gf GNumPred where
  gf GEven = mkApp (mkCId "Even") []
  gf GOdd = mkApp (mkCId "Odd") []
  gf GPrime = mkApp (mkCId "Prime") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Even" -> GEven 
      Just (i,[]) | i == mkCId "Odd" -> GOdd 
      Just (i,[]) | i == mkCId "Prime" -> GPrime 


      _ -> error ("no NumPred " ++ show t)

instance Gf GObject where
  gf (GNatObj x1) = mkApp (mkCId "NatObj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NatObj" -> GNatObj (fg x1)


      _ -> error ("no Object " ++ show t)

instance Gf GQuestion where
  gf (GIsNumPred x1 x2) = mkApp (mkCId "IsNumPred") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "IsNumPred" -> GIsNumPred (fg x1) (fg x2)


      _ -> error ("no Question " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GNoIsNumPred x1 x2 -> r GNoIsNumPred `a` f x1 `a` f x2
    GYesIsNumPred x1 x2 -> r GYesIsNumPred `a` f x1 `a` f x2
    GBinFun x1 x2 x3 -> r GBinFun `a` f x1 `a` f x2 `a` f x3
    GLstFun x1 x2 -> r GLstFun `a` f x1 `a` f x2
    GNumber x1 -> r GNumber `a` f x1
    GNatObj x1 -> r GNatObj `a` f x1
    GIsNumPred x1 x2 -> r GIsNumPred `a` f x1 `a` f x2
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
