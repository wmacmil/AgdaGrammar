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
type GConj = Tree GConj_
data GConj_
type GFun2 = Tree GFun2_
data GFun2_
type GListNat = Tree GListNat_
data GListNat_
type GListNumPred = Tree GListNumPred_
data GListNumPred_
type GListProp = Tree GListProp_
data GListProp_
type GNat = Tree GNat_
data GNat_
type GNumPred = Tree GNumPred_
data GNumPred_
type GObject = Tree GObject_
data GObject_
type GProp = Tree GProp_
data GProp_
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
  GNoProp :: GProp -> Tree GAnswer_
  GYes :: Tree GAnswer_
  GYesProp :: GProp -> Tree GAnswer_
  GAnd :: Tree GConj_
  GOr :: Tree GConj_
  GPlus :: Tree GFun2_
  GTimes :: Tree GFun2_
  GListNat :: [GNat] -> Tree GListNat_
  GListNumPred :: [GNumPred] -> Tree GListNumPred_
  GListProp :: [GProp] -> Tree GListProp_
  GBinFun :: GFun2 -> GNat -> GNat -> Tree GNat_
  GLstFun :: GFun2 -> GListNat -> Tree GNat_
  GNumber :: GInt -> Tree GNat_
  GEven :: Tree GNumPred_
  GLstNumProp :: GConj -> GListNumPred -> Tree GNumPred_
  GOdd :: Tree GNumPred_
  GPrime :: Tree GNumPred_
  GNatObj :: GNat -> Tree GObject_
  GIf :: GProp -> GProp -> Tree GProp_
  GIsNumProp :: GNumPred -> GObject -> Tree GProp_
  GLstProp :: GConj -> GListProp -> Tree GProp_
  GNot :: GProp -> Tree GProp_
  GPConj :: GConj -> GProp -> GProp -> Tree GProp_
  GPropQuest :: GProp -> Tree GQuestion_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GNo,GNo) -> and [ ]
    (GNoProp x1,GNoProp y1) -> and [ x1 == y1 ]
    (GYes,GYes) -> and [ ]
    (GYesProp x1,GYesProp y1) -> and [ x1 == y1 ]
    (GAnd,GAnd) -> and [ ]
    (GOr,GOr) -> and [ ]
    (GPlus,GPlus) -> and [ ]
    (GTimes,GTimes) -> and [ ]
    (GListNat x1,GListNat y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListNumPred x1,GListNumPred y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListProp x1,GListProp y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GBinFun x1 x2 x3,GBinFun y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GLstFun x1 x2,GLstFun y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNumber x1,GNumber y1) -> and [ x1 == y1 ]
    (GEven,GEven) -> and [ ]
    (GLstNumProp x1 x2,GLstNumProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GOdd,GOdd) -> and [ ]
    (GPrime,GPrime) -> and [ ]
    (GNatObj x1,GNatObj y1) -> and [ x1 == y1 ]
    (GIf x1 x2,GIf y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GIsNumProp x1 x2,GIsNumProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GLstProp x1 x2,GLstProp y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GNot x1,GNot y1) -> and [ x1 == y1 ]
    (GPConj x1 x2 x3,GPConj y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GPropQuest x1,GPropQuest y1) -> and [ x1 == y1 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAnswer where
  gf GNo = mkApp (mkCId "No") []
  gf (GNoProp x1) = mkApp (mkCId "NoProp") [gf x1]
  gf GYes = mkApp (mkCId "Yes") []
  gf (GYesProp x1) = mkApp (mkCId "YesProp") [gf x1]

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "No" -> GNo 
      Just (i,[x1]) | i == mkCId "NoProp" -> GNoProp (fg x1)
      Just (i,[]) | i == mkCId "Yes" -> GYes 
      Just (i,[x1]) | i == mkCId "YesProp" -> GYesProp (fg x1)


      _ -> error ("no Answer " ++ show t)

instance Gf GConj where
  gf GAnd = mkApp (mkCId "And") []
  gf GOr = mkApp (mkCId "Or") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "And" -> GAnd 
      Just (i,[]) | i == mkCId "Or" -> GOr 


      _ -> error ("no Conj " ++ show t)

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

instance Gf GListNumPred where
  gf (GListNumPred [x1,x2]) = mkApp (mkCId "BaseNumPred") [gf x1, gf x2]
  gf (GListNumPred (x:xs)) = mkApp (mkCId "ConsNumPred") [gf x, gf (GListNumPred xs)]
  fg t =
    GListNumPred (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseNumPred" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsNumPred" -> fg x1 : fgs x2


      _ -> error ("no ListNumPred " ++ show t)

instance Gf GListProp where
  gf (GListProp [x1,x2]) = mkApp (mkCId "BaseProp") [gf x1, gf x2]
  gf (GListProp (x:xs)) = mkApp (mkCId "ConsProp") [gf x, gf (GListProp xs)]
  fg t =
    GListProp (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseProp" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsProp" -> fg x1 : fgs x2


      _ -> error ("no ListProp " ++ show t)

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
  gf (GLstNumProp x1 x2) = mkApp (mkCId "LstNumProp") [gf x1, gf x2]
  gf GOdd = mkApp (mkCId "Odd") []
  gf GPrime = mkApp (mkCId "Prime") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Even" -> GEven 
      Just (i,[x1,x2]) | i == mkCId "LstNumProp" -> GLstNumProp (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "Odd" -> GOdd 
      Just (i,[]) | i == mkCId "Prime" -> GPrime 


      _ -> error ("no NumPred " ++ show t)

instance Gf GObject where
  gf (GNatObj x1) = mkApp (mkCId "NatObj") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "NatObj" -> GNatObj (fg x1)


      _ -> error ("no Object " ++ show t)

instance Gf GProp where
  gf (GIf x1 x2) = mkApp (mkCId "If") [gf x1, gf x2]
  gf (GIsNumProp x1 x2) = mkApp (mkCId "IsNumProp") [gf x1, gf x2]
  gf (GLstProp x1 x2) = mkApp (mkCId "LstProp") [gf x1, gf x2]
  gf (GNot x1) = mkApp (mkCId "Not") [gf x1]
  gf (GPConj x1 x2 x3) = mkApp (mkCId "PConj") [gf x1, gf x2, gf x3]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "If" -> GIf (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "IsNumProp" -> GIsNumProp (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "LstProp" -> GLstProp (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "Not" -> GNot (fg x1)
      Just (i,[x1,x2,x3]) | i == mkCId "PConj" -> GPConj (fg x1) (fg x2) (fg x3)


      _ -> error ("no Prop " ++ show t)

instance Gf GQuestion where
  gf (GPropQuest x1) = mkApp (mkCId "PropQuest") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "PropQuest" -> GPropQuest (fg x1)


      _ -> error ("no Question " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GNoProp x1 -> r GNoProp `a` f x1
    GYesProp x1 -> r GYesProp `a` f x1
    GBinFun x1 x2 x3 -> r GBinFun `a` f x1 `a` f x2 `a` f x3
    GLstFun x1 x2 -> r GLstFun `a` f x1 `a` f x2
    GNumber x1 -> r GNumber `a` f x1
    GLstNumProp x1 x2 -> r GLstNumProp `a` f x1 `a` f x2
    GNatObj x1 -> r GNatObj `a` f x1
    GIf x1 x2 -> r GIf `a` f x1 `a` f x2
    GIsNumProp x1 x2 -> r GIsNumProp `a` f x1 `a` f x2
    GLstProp x1 x2 -> r GLstProp `a` f x1 `a` f x2
    GNot x1 -> r GNot `a` f x1
    GPConj x1 x2 x3 -> r GPConj `a` f x1 `a` f x2 `a` f x3
    GPropQuest x1 -> r GPropQuest `a` f x1
    GListNat x1 -> r GListNat `a` foldr (a . a (r (:)) . f) (r []) x1
    GListNumPred x1 -> r GListNumPred `a` foldr (a . a (r (:)) . f) (r []) x1
    GListProp x1 -> r GListProp `a` foldr (a . a (r (:)) . f) (r []) x1
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
