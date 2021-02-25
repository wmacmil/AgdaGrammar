concrete QueryEng of Query = open Prelude in {

lincat
  Answer , Question , Object , Nat = SS ;

  -- ListNat = SS ;
  [Nat] = SS ;
  Fun2 = SS ;

lin

--Yes : Answer ;
  Yes = ss "yes" ;
--No : Answer ;
  No = ss "no" ;

--IsEven  : Object -> Question ;
  IsEven = mkQuestion "even" ;
--IsOdd   : Object -> Question ;
  IsOdd = mkQuestion "odd" ;
--IsPrime : Object -> Question ;
  IsPrime = mkQuestion "prime" ;

--NatObj : Nat -> Object ;
  NatObj n = n ;

--Number : Int -> Nat ;
  Number i = i ;

  Plus = ss "the sum of" ;
  Times = ss "the product of" ;

  BinFun f n1 n2 = ss (f.s ++ n1.s ++ "and" ++ n2.s) ;

--ListFun  : Fun2 -> ListNat -> Nat ;
  ListFun f ls = ss (f.s ++ ls.s) ;

--BaseNat : Nat -> ListNat ;
  BaseNat n1 n2 = ss (n1.s ++ "and" ++ n2.s) ;

--ConsNat : Nat -> ListNat -> ListNat ;
  ConsNat n ls = ss (n.s ++ "," ++ ls.s) ;

  ----test cases
  -- p -cat=Nat "the product of 9 , 8 and 7"
  --   ListFun Times (ConsNat (Number 9) (BaseNat (Number 8) (Number 7)))

oper
  -- SS arg is the object
  mkQuestion : Str -> SS -> SS ;
  mkQuestion str s = ss ("is" ++ s.s ++ str ++ "?") ;

  --generalize this to overload, for instance, are 3 and 4 equal
  -- does 3 = 4 ?

}
