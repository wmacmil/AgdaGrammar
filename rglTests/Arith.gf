abstract Arith = {

  flags startcat = Prop ; 

cat 
  Prop ; Ind ; Var ;

fun
  And, Or, If   : Prop -> Prop -> Prop ;
  Not           : Prop -> Prop ;
  Forall, Exist : Var  -> Prop -> Prop ;
  IVar          : Var  -> Ind ;
  VStr          : String -> Var ;


  IInt           : Int -> Ind ;
  Add, Mul       : Ind -> Ind -> Ind ;
  Nat, Even, Odd : Ind -> Prop ;
  Equal          : Ind -> Ind -> Prop ;

}
