resource Helpers = open
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  ExtraEng, -- for the negative prop
  Prelude in {

param
  Polr = Pos | Neg ;
  Compl = Simp | Com ; -- True == Com

oper
  if_then_elseC : (A : Type) -> Compl -> A -> A -> A = \_,c,d,e ->
    case c of {
      Com => d ;
      Simp => e
    } ;

  orC  : (_,_ : Compl) -> Compl = \a,b -> if_then_elseC Compl a Com b ;


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
          (mkPhr (mkS (polrTrans pol) (mkCl obj even))) fullStopPunct ) ;
    yesno : Utt -> S -> Text =
      \utt, s ->
      mkText
        (mkPhr utt)
        fullStopPunct
        (mkText
          (mkPhr s) fullStopPunct )
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
  hold_V = mkV "hold" "held" "held" ;

  theCaseThat : S -> Cl ;
  theCaseThat p = (mkCl (mkVP (mkNP the_Quant (mkCN case_N (ConstructorsEng.mkAdv that_Subj p))))) ;

  negate : S -> S ;
  negate s =
    (mkS
       ExtraEng.UncNeg
       (theCaseThat s)) ;

  bulletConj = lin Conj {s1,s2 = "\\item" ; n = singular} ;
  colonConj = lin Conj {s1 = [] ; s2 = ":" ; n = singular} ;

}
