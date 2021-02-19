abstract Arith = {

  flags startcat = Prop ; 

cat 
  Prop ; Ind ; Var ; Conj ;

fun
  If            : Prop -> Prop -> Prop ;
  Not           : Prop -> Prop ;
  Forall, Exist : Var  -> Prop -> Prop ;
  IVar          : Var  -> Ind ;
  VStr          : String -> Var ;

  PConj : Conj -> Prop -> Prop -> Prop ;
  And, Or : Conj ;

  IInt           : Int -> Ind ;
  Add, Mul       : Ind -> Ind -> Ind ;
  Nat, Even, Odd : Ind -> Prop ;
  Equal          : Ind -> Ind -> Prop ;

}
