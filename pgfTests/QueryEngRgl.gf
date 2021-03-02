concrete QueryEngRgl of Query = open
  SyntaxEng,
  SymbolicEng,
  -- SymbolEng,
  SentenceEng,
  ConstructorsEng,
  (P = ParadigmsEng), ExtraEng, Prelude in {

lincat
  Answer = Text ;
  -- Question = QS ; -- QCl ; --or QS ?
  Question = Utt ; -- QCl ; --or QS ?
  -- can make compound questions after this

  Object , Nat = NP ;

  -- ListNat = SS ;
  [Nat] = [NP] ;
  Fun2 = N ;

lin

  -- Query> p -cat=Answer "yes ."

  Yes = yesno yes_Utt ;
  No = yesno no_Utt ;

  -- mkUtt (mkQS conditionalTense anteriorAnt negativePol (mkQCl who_IP sleep_V))
  -- mkUtt (mkQS (mkCl she_NP sleep_V))

--IsEven  : Object -> Question ;

  IsEven obj = mkUtt (mkQS (mkCl obj (mkAP (P.mkA "even")))) ;

  -- Even x =
  --   mkS (mkCl x (mkAP (P.mkA "even"))) ;

-- --IsOdd   : Object -> Question ;
--   IsOdd = mkQuestion "odd" ;
-- --IsPrime : Object -> Question ;
--   IsPrime = mkQuestion "prime" ;

--NatObj : Nat -> Object ;
  NatObj n = n ;

--Number : Int -> Nat ;
  Number = symb ;

  -- IInt = symb ;
  -- Plus  = app (P.mkN2 (P.mkN "sum")) ;
  Plus  = P.mkN "sum" ;
  Times = P.mkN "product" ;

  BinFun f n1 n2 = app (P.mkN2 f) n1 n2 ;

-- --ListFun  : Fun2 -> ListNat -> Nat ;
--   ListFun f ls = ss (f.s ++ ls.s) ;

-- --BaseNat : Nat -> ListNat ;
--   BaseNat n1 n2 = ss (n1.s ++ "and" ++ n2.s) ;

-- --ConsNat : Nat -> ListNat -> ListNat ;
--   ConsNat n ls = ss (n.s ++ "," ++ ls.s) ;

--   ----test cases
--   -- p -cat=Nat "the product of 9 , 8 and 7"
--   --   ListFun Times (ConsNat (Number 9) (BaseNat (Number 8) (Number 7)))

-- oper
--   -- SS arg is the object
--   mkQuestion : Str -> SS -> SS ;
--   mkQuestion str s = ss ("is" ++ s.s ++ str ++ "?") ;

--   --generalize this to overload, for instance, are 3 and 4 equal
--   -- does 3 = 4 ?

-- }

oper
  yesno : Utt -> Text ;
  yesno utt = mkText (mkPhr utt) fullStopPunct ;

  -- mkBin : Str  -> ?
  -- mkBin sum = app (P.mkN2 (P.mkN sum)) ;
}
