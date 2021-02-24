module Query where

import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GAnswer =
   GNo 
 | GYes 
  deriving Show

data GNat =
   GNumber GInt 
 | GPlus GNat GNat 
 | GTimes GNat GNat 
  deriving Show

data GObject = GNatObj GNat 
  deriving Show

data GQuestion =
   GIsEven GObject 
 | GIsOdd GObject 
 | GIsPrime GObject 
  deriving Show


instance Gf GAnswer where
  gf GNo = mkApp (mkCId "No") []
  gf GYes = mkApp (mkCId "Yes") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "No" -> GNo 
      Just (i,[]) | i == mkCId "Yes" -> GYes 


      _ -> error ("no Answer " ++ show t)

instance Gf GNat where
  gf (GNumber x1) = mkApp (mkCId "Number") [gf x1]
  gf (GPlus x1 x2) = mkApp (mkCId "Plus") [gf x1, gf x2]
  gf (GTimes x1 x2) = mkApp (mkCId "Times") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "Number" -> GNumber (fg x1)
      Just (i,[x1,x2]) | i == mkCId "Plus" -> GPlus (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "Times" -> GTimes (fg x1) (fg x2)


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


