abstract Query = {

flags startcat=Question ;

cat
  Answer ; Question ; Object ; Nat ;


  -- ListNat ; --{2}
  [Nat]{2};
  Fun2 ;

fun

  Yes : Answer ;
  No : Answer ;

  IsEven  : Object -> Question ;
  IsOdd   : Object -> Question ;
  IsPrime : Object -> Question ;

  NatObj : Nat -> Object ;

  Number : Int -> Nat ;

  Plus : Fun2 ;
  Times : Fun2 ;

  BinFun : Fun2 -> Nat -> Nat -> Nat ;

  -----------------------------------------
  -- but why not just have it unary so it just appends an and at the end
  -- BaseNat : Nat -> Nat -> ListNat ;

  ListFun  : Fun2 -> [Nat] -> Nat ;
  -- ListFun  : Fun2 -> ListNat -> Nat ;

  -- BaseNat : Nat -> Nat -> ListNat ;
  -- ConsNat : Nat -> ListNat -> ListNat ;



  -- Plus   : Nat -> Nat -> Nat ;
  -- Times  : Nat -> Nat -> Nat ;

  -- [Ind] {2} ;
  -- IFunC  : Fun2 -> [Ind] -> Ind ;
  -- p -cat=Ind "the product of x , y and z"


  -- could further refine this by including
  -- Cat NatProp ;
  -- Even , Odd , Prime : NatProp ;
  -- IsProp : NatProp -> Object -> Question ;

  -- could even further refine using depedent types 
  -- where one then introduces Kinds (Nat, Int, Vector Space) and
  -- and then have Props, Objections and questions depedent on Kind
  
}

