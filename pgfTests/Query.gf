abstract Query = {

flags startcat=Question ;

cat
  Answer ; Question ; Object ; Nat ;
fun

  IsEven  : Object -> Question ;
  IsOdd   : Object -> Question ;
  IsPrime : Object -> Question ;

  NatObj : Nat -> Object ;

  Plus   : Nat -> Nat -> Nat ;
  Times  : Nat -> Nat -> Nat ;

  Number : Int -> Nat ;

  Yes : Answer ;
  No : Answer ;

  -- could further refine this by including
  -- Cat NatProp ;
  -- Even , Odd , Prime : NatProp ;
  -- IsProp : NatProp -> Object -> Question ;

  -- could even further refine using depedent types 
  -- where one then introduces Kinds (Nat, Int, Vector Space) and
  -- and then have Props, Objections and questions depedent on Kind
  
}

