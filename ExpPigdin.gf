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
  [AIdent],
  [Decl] ,
  [Tele],
  [Branch] ,
  [PTele],
  [Label]
    = Str ;
  Exp = Str ; --TermPrec ;

lin

  DeclDef a lt e ew =
    "Theorem" ++ a ++ ".\n" ++ "Claim : " ++ lt ++ "whence" ++ e ++ ".\n" ++ "Proof :" ++ ew ++ "." ;
  DeclData a t d = "Definition :" ++ a ++ "parameterized by" ++ t ++ "is a Type freely generated by the following constructors" ++ d ;
  DeclSplit ai lt e lb = ai ++ lt ++ ":" ++ usePrec 0 e ++ "= split" ++ lb ;
    "Theorem" ++ ai ++ ".\n" ++ "Claim : " ++ lt ++ "whence" ++ e ++ ".\n" ++ "Proof :" ++ "We induct on" ++ ew ++ "." ; -- somehow need to know this first arguement of exp
  DeclUndef a lt e = "Axiom of"  ++ a ++ "." ++ "Given" ++ lt ++ "there yields" e "." ;

  Where e ld = e ++ "where we auxilliarily define" ++ ld ;
  NoWhere e = e ;

  Let ld e = "Let's use the following auxilliary definitions:" ++ ld ++ "in" ++ e ;
  Split e lb = "We induct on" ++ e ++ "in the follow cases" ++ lb ; --define by recursion
  Lam pt e = "If" ++ pt ++ "then" ++ e ;
  Fun e1 e2 = "a function from" ++ e1 ++ "to" ++ e2 ; -- A -> Set
  Pi pt e = "for all" ++ pt ++ "," ++ e ;
  --for every, cf HoTT libary 
  Sigma pt e = "there exists" ++ pt ++ "such that" ++ e ;
  App e1 e2 = "apply" ++ e1 ++ "to" ++ e2 ;
  Id e1 e2 e3 = e2 ++ "equals" ++ e3 ++ "as elements of" ++ e1 ;
-- for an explicit vs implicit use of parameters, may have to use expressions as records, with a parameter is_implicit
  IdJ e1 e2 e3 e4 e5 = ("Apply the identity eliminator to the predicate" ++ e1  "where..." ++ e2 ++ "on elements" ++ e3 ++ "andJ" ++ e4 ++ "and equality" e5 ;
  Fst e = "the first projection of" ++ e ;
  Snd e = "the second projection of" ++ e ;
  Pair e1 e2 = "the pair of" ++ e1 ++ "andPair" ++ e2 ;
  Var a = a ; -- the variable
  Univ = "Type" ;
  Refl = "reflexivity" ;

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

  Foo = "foo" ; 

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

  NegB = "negb" ;

  -- p "foo ( b : bool ) : bool = f b where f : bool -> bool = negb"

}
