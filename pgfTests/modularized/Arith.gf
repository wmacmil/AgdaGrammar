abstract Arith = {

flags startcat=Object ;

cat

  Object ;
  Nat ;
  NumPred ;
  Fun2 ;

fun

  NatObj : Nat -> Object ;

  -- unary
  Even , Odd, Prime : NumPred ;

  BinFun : Fun2 -> Nat -> Nat -> Nat ;
  Plus : Fun2 ;
  Times : Fun2 ;
  Number : Int -> Nat ;
  --Int is a built-in, GF primitive

}
