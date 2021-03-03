concrete QueryEngRgl of Query = open
  SyntaxEng,
  SymbolicEng,
  -- SymbolEng,
  SentenceEng,
  ConstructorsEng,
  (P = ParadigmsEng), ExtraEng, Prelude in {

lincat
  Answer = Text ;
  Object , Nat = NP ;
  Question = Utt ;
  [Nat] = [NP] ;
  Fun2 = N ;

lin

  Yes = yesno yes_Utt ;
  No = yesno no_Utt ;

  -- so need to add a polarity field for yes_Utt
--YesIsEven : Object -> Answer ;
  YesIsEven = yesno yes_Utt "even" ;
--NoIsEven : Object -> Answer ;
  -- NoIsEven obj = yesno no_Utt obj ;

--IsEven  : Object -> Question ;
--IsOdd   : Object -> Question ;
--IsPrime : Object -> Question ;
  IsEven = isNumericProp "even" ;
  IsOdd = isNumericProp "odd" ;
  IsPrime = isNumericProp "prime" ;

--NatObj : Nat -> Object ;
  NatObj n = n ;
  
--Number : Int -> Nat ;
  Number = symb ;

  Plus  = P.mkN "sum" ;
  Times = P.mkN "product" ;

  BinFun f = app (P.mkN2 f) ;

--ListFun  : Fun2 -> ListNat -> Nat ;
  ListFun f ls = app (P.mkN2 f) (mkNP and_Conj ls) ;

--BaseNat : Nat -> Nat -> ListNat ;
--ConsNat : Nat -> ListNat -> ListNat ;
  BaseNat = mkListNP ;
  ConsNat = mkListNP ;

oper

  -- yesno : Utt -> Text ;
  -- yesno utt = mkText (mkPhr utt) fullStopPunct ;

  yesno = overload {
    yesno : Utt -> Text
      = \utt -> mkText (mkPhr utt) fullStopPunct ;
    yesno : Utt -> Str -> NP -> Text =
      \utt -> \str -> \obj ->
      mkText
        (mkPhr utt)
        fullStopPunct
        (mkText
          (mkPhr (mkCl obj (mkAP (P.mkA str)))) fullStopPunct ) ;
  } ;

  -- isNumericCl : Str -> NP -> Utt ;
  -- isNumericCl even obj = mkCl obj (mkAP (P.mkA even)) ;

  isNumericProp : Str -> NP -> Utt ;
  isNumericProp even obj = mkUtt (mkQS (mkCl obj (mkAP (P.mkA even)))) ;

  -- mkBin : Str  -> ?
  -- mkBin sum = app (P.mkN2 (P.mkN sum)) ;

  -- mkText (mkPhr (mkQS (mkCl she_NP sleep_V))) questMarkPunct (mkText (mkPhr yes_Utt) fullStopPunct)
  -- mkText (mkPhr yes_Utt) questMarkPunct

}
