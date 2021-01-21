concrete ExpEng of Exp = open Prelude, FormalTwo in {

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

  DeclDef a lt e ew = a ++ lt ++ ":" ++ e.s ++ "=" ++ ew ;
  DeclData a t d = "data" ++ a ++ t ++ ": Set where" ++ d ;
  DeclSplit ai lt e lb = ai ++ lt ++ ":" ++ e.s ++ "= split" ++ lb ;
  DeclUndef a lt e = a ++ lt ++ ":" ++ e.s ++ "= undefined" ; -- postulate in agda

  Where e ld = e.s ++ "where" ++ ld ;
  NoWhere e = e.s ;

  -- Let ld e = mkPrec 0 ("let" ++ ld ++ "in" ++ (usePrec 0 e.s)) ;
  Let ld e = mkPrec 0 ("let" ++ ld ++ "in" ++ (usePrec 0 e)) ;
  Lam pt e = mkPrec 0 ("\\" ++ pt ++ "->" ++ usePrec 0 e) ;
  Fun = infixr 1 "->" ; -- A -> Set
  Pi pt e = mkPrec 1 (pt ++ "->" ++ usePrec 1 e) ;
  Sigma pt e = mkPrec 1 (pt ++ "*" ++ usePrec 1 e) ;
  App = infixl 2 "" ;
  Id e1 e2 e3 = mkPrec 3 (usePrec 4 e1 ++ usePrec 4 e2 ++ "==" ++ usePrec 3 e2) ;
  IdJ e1 e2 e3 e4 e5 = mkPrec 3 ("J" ++ usePrec 4 e1 ++ usePrec 4 e2 ++ usePrec 4 e3 ++ usePrec 4 e4 ++ usePrec 4 e5) ;
  Fst e = mkPrec 4 ("proj1" ++ usePrec 4 e) ;
  Snd e = mkPrec 4 ("proj2" ++ usePrec 4 e) ;
  Pair e1 e2 = mkPrec 5 (usePrec 0 e1 ++ "," ++ usePrec 0 e2) ; --i got rid of parens
  Var a = constant a ;
  U = constant "U" ;

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
  TeleC a la e = "(" ++ a ++ la ++ ":" ++ e.s ++ ")" ;
  PTeleC e1 e2 = "(" ++ top e1 ++ ":" ++ top e2 ++ ")" ;

  OLabel a lt = a ++ lt ;

  --object language syntax, all variables for now

  X = "x" ;
  Y = "y" ;
  Z = "z" ;
  B = "b" ;

  Bool = "bool" ;
  True = "true" ;
  False = "false" ;
  CaseBool = "caseBool" ;
  IndBool = "indBool" ;


}
