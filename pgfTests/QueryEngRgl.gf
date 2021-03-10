concrete QueryEngRgl of Query = open
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  Prelude in {

lincat
  Answer = Text ;
  Object , Nat = NP ;
  Question = Utt ;
  [Nat] = [NP] ;
  Fun2 = N ;
  NumPred = AP ;


lin

--Even , Odd, Prime : NumPred ;
  Even = numprop "even" ;
  Odd = numprop "odd" ;
  Prime = numprop "prime" ;

--YesIsNumPred : NumPred -> Object -> Answer ;
--NoIsNumPred : NumPred -> Object -> Answer ;
  YesIsNumPred = yesno Pos yes_Utt ;
  NoIsNumPred = yesno Neg no_Utt ;

  Yes = yesno yes_Utt ;
  No = yesno no_Utt ;

--IsNumPred : NumPred -> Object -> Question ;
  IsNumPred = isNumericPred ;

--NatObj : Nat -> Object ;
  NatObj n = n ;

--Number : Int -> Nat ;
  Number = symb ;

  Plus  = mkN "sum" ;
  Times = mkN "product" ;

  BinFun f = app (mkN2 f) ;

--ListFun  : Fun2 -> ListNat -> Nat ;
  LstFun f ls = app (mkN2 f) (mkNP and_Conj ls) ;

--BaseNat : Nat -> Nat -> ListNat ;
--ConsNat : Nat -> ListNat -> ListNat ;
  BaseNat = mkListNP ;
  ConsNat = mkListNP ;

param Polr = Pos | Neg ;

oper

  polrTrans : Polr -> Pol ;
  polrTrans p = case p of {
    Pos => positivePol ;
    Neg => negativePol 
  } ;

  yesno = overload {
    yesno : Utt -> Text
      = \utt -> mkText (mkPhr utt) fullStopPunct ;
    yesno : Polr -> Utt -> AP -> NP -> Text =
      \pol, utt, even, obj ->
      mkText
        (mkPhr utt)
        fullStopPunct
        (mkText
          (mkPhr (mkS (polrTrans pol) (mkCl obj even))) fullStopPunct )
  } ;

  --refactor to include question mark
  isNumericPred : AP -> NP -> Utt ;
  isNumericPred even obj = mkUtt (mkQS (mkCl obj even)) ;

  -- number propery
  numprop : Str -> AP ;
  numprop even = mkAP (mkA even) ;

}
