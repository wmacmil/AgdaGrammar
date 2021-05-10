abstract Prop = Arith ** {

flags startcat=Prop ;

cat

  Conj;
  Prop;

fun

  IsNumProp : NumPred -> Object -> Prop ;

  If            : Prop -> Prop -> Prop ;
  Not           : Prop -> Prop ;
  PConj : Conj -> Prop -> Prop -> Prop ;

  And, Or : Conj ;

}
