concrete ArithEng of Arith = open
  Helpers,
  -- rgl
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  ExtraEng, -- for the negative prop
  Prelude in {

lincat

  -- Answer = Text ;
  -- Question = Utt ;

  Object , Nat = NP ;

  Fun2 = N ;
  NumPred = AP ;

  -- Prop = { s : S ; c : Compl } ;
  -- Conj = { s : SyntaxEng.Conj ; c : S }  ;

  -- [Prop] = { s : [S] ; c : Compl } ; --TODO add extra parameter
  -- [Nat] = [NP] ;
  -- [NumPred] = [AP] ;
  -- -- [Prop] = { s : Compl => [S] ; c : Compl } ; -- add commas or bullets depending on the compl


lin

--Even , Odd, Prime : NumPred ;
  Even = numprop "even" ;
  Odd = numprop "odd" ;
  Prime = numprop "prime" ;

  BinFun f = app (mkN2 f) ;

--NatObj : Nat -> Object ;
  NatObj n = n ;

--Number : Int -> Nat ;
  Number = symb ;

  Plus  = mkN "sum" ;
  Times = mkN "product" ;

}
