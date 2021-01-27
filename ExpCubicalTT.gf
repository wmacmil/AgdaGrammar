concrete ExpCubicalTT of Exp = open Prelude, FormalTwo in {

lincat 
  Comment,
  Module ,
  AIdent,
  Imp,
  Decl ,
  ExpWhere,
  Tele,
  Branch ,
  PTele,
  Label,
    -- = Str ;
  [AIdent],
  [Decl] ,
  -- [Exp],
  [Tele],
  [Branch] ,
  [PTele],
  [Label]
    -- = {hd,tl : Str} ;
    = Str ;
  Exp = TermPrec ;

lin

  DeclDef a lt e ew = a ++ lt ++ ":" ++ usePrec 0 e ++ "=" ++ ew ;
  DeclData a t d = "data" ++ a ++ t ++ ": Set where" ++ d ;
  DeclSplit ai lt e lb = ai ++ lt ++ ":" ++ usePrec 0 e ++ "= split" ++ lb ;
  DeclUndef a lt e = a ++ lt ++ ":" ++ usePrec 0 e ++ "= undefined" ; -- postulate in agda

  Where e ld = usePrec 0 e ++ "where" ++ ld ;
  NoWhere e = usePrec 0 e ;

  Let ld e = mkPrec 0 ("let" ++ ld ++ "in" ++ (usePrec 0 e)) ;
  Split e lb = mkPrec 0 ("split@" ++ usePrec 0 e ++ "with" ++ lb) ;
  Lam pt e = mkPrec 0 ("\\" ++ pt ++ "->" ++ usePrec 0 e) ;
  Fun = infixr 1 "->" ; -- A -> Set
  Pi pt e = mkPrec 1 (pt ++ "->" ++ usePrec 1 e) ;
  Sigma pt e = mkPrec 1 (pt ++ "*" ++ usePrec 1 e) ;
  App = infixl 2 "" ;
  Id e1 e2 e3 = mkPrec 3 (usePrec 4 e1 ++ usePrec 4 e2 ++ "==" ++ usePrec 3 e3) ;
  -- Id e1 e2 e3 = mkPrec 3 ("Id" ++ usePrec 4 e1 ++ usePrec 4 e2 ++ usePrec 3 e3) ;
  IdJ e1 e2 e3 e4 e5 = mkPrec 3 ("J" ++ usePrec 4 e1 ++ usePrec 4 e2 ++ usePrec 4 e3 ++ usePrec 4 e4 ++ usePrec 4 e5) ;
  Fst e = mkPrec 4 ("proj1" ++ usePrec 4 e) ;
  Snd e = mkPrec 4 ("proj2" ++ usePrec 4 e) ;
  Pair e1 e2 = mkPrec 5 ("(" ++ usePrec 0 e1 ++ "," ++ usePrec 0 e2 ++ ")") ;
  Var a = constant a ;
  Univ = constant "Set" ;
  Refl = constant "refl" ;

  BaseAIdent = "" ;
  ConsAIdent x xs = x ++ xs ;

  -- [Decl] only used in ExpWhere
  BaseDecl x = x ;
  ConsDecl x xs = x ++ "\n" ++ xs ;

  -- maybe accomodate so split on empty type just gives () 
  -- BaseBranch = "" ;
  BaseBranch x = x ;
  -- ConsBranch x xs = x ++ "\n" ++ xs ;
  ConsBranch x xs = x ++ "||" ++ xs ;

  -- for data constructors
  BaseLabel x = x ;
  ConsLabel x xs = x ++ "|" ++ xs ; 

  BasePTele x = x ;
  ConsPTele x xs = x ++ xs ;

  BaseTele = "" ;
  ConsTele x xs = x ++ xs ;

  OBranch a la ew = a ++ la ++ "->" ++ ew ;
  TeleC a la e = "(" ++ a ++ la ++ ":" ++ usePrec 0 e ++ ")" ;
  PTeleC e1 e2 = "(" ++ top e1 ++ ":" ++ top e2 ++ ")" ;

  OLabel a lt = a ++ lt ;

  --object language syntax, all variables for now

  Bool = "bool" ;
  True = "true" ;
  False = "false" ;
  CaseBool = "caseBool" ;
  IndBool = "indBool" ;
  FunExt = "funExt" ;

  Nat = "nat" ;
  Zero = "zero" ;
  Suc = "suc" ;
  EqualNat = "equalNat" ;

  Unit = "unit" ;
  Top = "top" ;

  A = "a" ;
  B = "b" ;
  C = "c" ;
  D = "d" ;
  E = "e" ;
  F = "f" ;
  G = "g" ;
  H = "h" ;
  I = "i" ;
  J = "j" ;
  K = "k" ;
  L = "l" ;
  M = "m" ;
  N = "n" ;
  O = "o" ;
  P = "p" ;
  Q = "q" ;
  R = "r" ;
  S = "s" ;
  T = "t" ;
  U = "u" ;
  V = "v" ;
  W = "w" ;
  X = "x" ;
  Y = "y" ;
  Z = "z" ;

  -- p "equalNat : nat -> nat -> bool = split zero -> split@ ( nat -> bool ) with zero  -> true || suc n -> false || suc m -> split@ ( nat -> bool ) with zero -> false || suc n -> equalNat m n"

  -- p "equalNat : top -> nat -> bool = split unit -> split@ ( nat -> bool ) with zero  -> true || suc n -> false"

  -- || suc m -> split@ ( nat -> bool ) zero -> false || suc n -> equalNat m n"


  -- data nat = zero | suc (n : nat)

  -- p "funExt  ( a : Set )   ( b : a -> Set )   ( f :  ( x : a )  -> b x )   ( p :  ( x : a )  -> ( b x )   ( f x ) == ( g x )  )  : (  ( y : a )  -> b y )  f == g = undefined
  -- p "funExt  ( a : Set )   ( b : a -> Set )   ( f g :  ( x : a )  -> b x ) ( p :  ( ( x : a )  -> ( b x ) ) ( f x ) == ( g x )  ) : bool = undefined "

  -- ( p :  ( x : a )  -> ( b x )   ( f x ) == ( g x )  )  : (  ( y : a )  -> b y )  f == g = undefined

}
