abstract PropExt = Prop ** {

flags startcat=Prop ;

cat
  
  [Prop]{2};

  [Nat]{2};
  [NumPred]{2};

fun

  --Prop extend
  LstProp : Conj  -> [Prop] -> Prop ;

  --Arith extend
  LstNumProp : Conj -> [NumPred] -> NumPred ;
  LstFun  : Fun2 -> [Nat] -> Nat ;

}
