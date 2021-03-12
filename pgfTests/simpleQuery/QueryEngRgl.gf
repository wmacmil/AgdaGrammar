concrete QueryEngRgl of Query = open
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  ExtraEng, -- for the negative prop
  Prelude in {

lincat
  Answer = Text ;
  Object , Nat = NP ;
  Question = Utt ;
  [Nat] = [NP] ;
  Fun2 = N ;
  NumPred = AP ;

  Prop = S ;
  Conj = SyntaxEng.Conj  ;

lin

  -- PropQuest : Prop -> Question ;

--IsNumProp : NumPred -> Object -> Prop ;
  IsNumProp odd obj = mkS (mkCl obj odd) ;

  -- Odd x =
  --   mkS (mkCl x (mkAP (P.mkA "odd"))) ;
  --from yesno (mkPhr (mkS (polrTrans pol) (mkCl obj even))) fullStopPunct )

--If : Prop -> Prop -> Prop ;
  If p q = mkS (ConstructorsEng.mkAdv if_Subj p) (mkS then_Adv q) ;

  -- Not           : Prop -> Prop ;
  Not p =
    mkS
      ExtraEng.UncNeg
      (mkCl
         (mkVP (mkNP the_Quant (mkCN case_N (ConstructorsEng.mkAdv that_Subj p))))) ;

--PConj : Conj -> Prop -> Prop -> Prop ;
  PConj = mkS ;

--And, Or : Conj ;
  And = and_Conj ;
  Or = or_Conj ;

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

  then_Adv = ParadigmsEng.mkAdv "then" ;
  such_A = mkA "such" ;
  case_N = mkN "case" ;

}
