concrete QueryEng of Query = open Prelude in {

lincat
  Answer , Question , Object , Nat = SS ;

lin

--IsEven  : Object -> Question ;
  IsEven = mkQuestion "even" ;
--IsOdd   : Object -> Question ;
  IsOdd = mkQuestion "odd" ;
--IsPrime : Object -> Question ;
  IsPrime = mkQuestion "prime" ;

--NatObj : Nat -> Object ;
  NatObj n = n ;

--Plus   : Nat -> Nat -> Nat ;
  Plus n1 n2 = ss (n1.s ++ "+" ++ n2.s) ;
--Times  : Nat -> Nat -> Nat ;
  Times n1 n2 = ss (n1.s ++ "*" ++ n2.s) ;

  -- Number : Int -> Nat ;
  Number i = i ;

--Yes : Answer ;
  Yes = ss "yes" ;
--No : Answer ;
  No = ss "no" ;

oper
  -- SS arg is the object
  mkQuestion : Str -> SS -> SS ;
  mkQuestion str s = ss ("is" ++ s.s ++ str ++ "?") ;

}

