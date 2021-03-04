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

  YesIsEven : Object -> Answer ;
  NoIsEven : Object -> Answer ;

  IsEven  : Object -> Question ;
  IsOdd   : Object -> Question ;
  IsPrime : Object -> Question ;

  NatObj : Nat -> Object ;

  Number : Int -> Nat ;

  Plus : Fun2 ;
  Times : Fun2 ;

  BinFun : Fun2 -> Nat -> Nat -> Nat ;

  LstFun  : Fun2 -> [Nat] -> Nat ;

  -- BaseNat : Nat -> Nat -> ListNat ;
  -- ConsNat : Nat -> ListNat -> ListNat ;

  -----------------------------------------
  -- but why not just have it unary so it just appends an and at the end -- i think it has to do with commas

  -- could further refine this by including
  -- Cat NatProp ;
  -- Even , Odd , Prime : NatProp ;
  -- IsProp : NatProp -> Object -> Question ;

  -- could even further refine using depedent types
  -- where one then introduces Kinds (Nat, Int, Vector Space) and
  -- and then have Props, Objections and questions depedent on Kind

}
