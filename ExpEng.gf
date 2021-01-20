concrete ExpEng of Exp = open Prelude, Formal in {




lincat 

  Comment,
  Module ,
  AIdent,
  Imp,
  Decl ,
  Exp,
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

lin

  DeclDef a lt e ew = a ++ lt ++ ":" ++ e ++ "=" ++ ew ;
  
  -- why isn't this generating
  DeclData a t d = "data" ++ a ++ t ++ ": Set where" ++ d ;


  DeclSplit ai lt e lb = ai ++ lt ++ ":" ++ e ++ "= split" ++ lb ;
  DeclUndef a lt e = a ++ lt ++ ":" ++ e ++ "= undefined" ; -- postulate in agda

  Where e ld = e ++ "where" ++ ld ;
  NoWhere e = e ;

  Let ld e = "let" ++ ld ++ "in" ++ e ;
  Lam pt e = "\\" ++ pt ++ "->" ++ e ;

  -- Split e br = "case" ++ e ++ "of" ++ br ; 
  Fun e1 e2 = e1 ++ "->" ++ e2 ; -- A -> Set

  Pi pt e = pt ++ "->" ++ e ;
  Sigma pt e = pt ++ "*" ++ e ;
  App e1 e2 = e1 ++ e2 ;
  Id e1 e2 = e1 ++ "==" ++ e2 ;
  -- how to handle eta equlity - probably overloading, right
  IdJ e1 e2 e3 e4 e5 = "J" ++ e1 ++ e2 ++ e3 ++ e4 ++ e5 ;
  Fst e = "proj1" ++ e ;
  Snd e = "proj2" ++ e ;
  Pair e el = "(" ++ e ++ "," ++ el ++ ")" ;
  Var a = a ;
--  Var : AIdent -> Exp ;          

  U = "U" ;

  

--   PTeleC : Exp -> Exp -> Exp -> PTele ;

--   GenAIdent : String -> AIdent ;
  X = "x" ;
  Y = "y" ;
  Z = "z" ;

  BaseAIdent = "" ;
  ConsAIdent x xs = x ++ xs ;

  -- [Decl] only used in ExpWhere
  BaseDecl x = x ;
  ConsDecl x xs = x ++ "\n" ++ xs ;

  -- can split on tt, therefore just needs one arguement
  -- maybe accomodate so split on empty type just gives () 
  BaseBranch x = x ;
  ConsBranch x xs = x ++ "\n" ++ xs ;

  -- for data constructors
  BaseLabel x = x ;
  ConsLabel x xs = x ++ "|" ++ xs ; 
  -- ConsLabel x xs = x ++ xs ; 

  BasePTele x = x ;
  ConsPTele x xs = x ++ xs ;

  BaseTele = "" ;
  ConsTele x xs = x ++ xs ;

  OBranch a la ew = a ++ la ++ "->" ++ ew ;
  TeleC a la e = "(" ++ a ++ la ++ ":" ++ e ++ ")" ;
  PTeleC e1 e2 = "(" ++ e1 ++ ":" ++ e2 ++ ")" ;

  OLabel a lt = a ++ lt ;

  -- whatsup with this
  --   Exp> gr | l
  --     x ( x x : U U ) : ( proj1 U == U , proj2 z ) = proj1 U
  --     21204 msec


}
