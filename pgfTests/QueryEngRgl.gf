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
p
--ListFun  : Fun2 -> ListNat -> Nat ;
  ListFun f ls = app (P.mkN2 f) (mkNP and_Conj ls) ;

  -- mkUtt (mkNP or_Conj (mkListNP (mkNP this_Det woman_N) (mkListNP (mkNP john_PN) i_NP)))

--BaseNat : Nat -> ListNat ;
  BaseNat = mkListNP ; -- ss (n1.s ++ "and" ++ n2.s) ;
  ConsNat = mkListNP ; -- ss (n1.s ++ "and" ++ n2.s) ;

-- --ConsNat : Nat -> ListNat -> ListNat ;
--   ConsNat n ls = ss (n.s ++ "," ++ ls.s) ;

--   ----test cases
--   -- p -cat=Nat "the product of 9 , 8 and 7"
--   --   ListFun Times (ConsNat (Number 9) (BaseNat (Number 8) (Number 7)))

--   --generalize this to overload, for instance, are 3 and 4 equal
--   -- does 3 = 4 ?

oper
  yesno : Utt -> Text ;
  yesno utt = mkText (mkPhr utt) fullStopPunct ;

  isNumericProp : Str -> NP -> Utt ; 
  isNumericProp even obj = mkUtt (mkQS (mkCl obj (mkAP (P.mkA even)))) ;
  -- mkBin : Str  -> ?
  -- mkBin sum = app (P.mkN2 (P.mkN sum)) ;
}
