concrete PropExtEng of PropExt = ArithEng, PropEng ** open
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

  -- Object , Nat = NP ;
  -- -- Fun2 = N ;
  -- NumPred = AP ;

  -- Prop = { s : S ; c : Compl } ;
  -- Conj = { s : SyntaxEng.Conj ; c : S }  ;

  [Prop] = { s : [S] ; c : Compl } ; --TODO add extra parameter
  [Nat] = [NP] ;
  [NumPred] = [AP] ;
  -- [Prop] = { s : Compl => [S] ; c : Compl } ; -- add commas or bullets depending on the compl


lin

--LstProp : Conj  -> [Prop] -> Prop ; -- E
  LstProp c ps = case ps.c of {
    Com  => {s = mkS colonConj c.c (mkS bulletConj ps.s) ; c = Simp } ;
    Simp => {s = mkS c.s ps.s ; c = Com }
    } ;

--LstNumProp : Conj -> [NumPred] -> NumPred ; -- E
  LstNumProp c = mkAP c.s ;

--ListFun  : Fun2 -> ListNat -> Nat ;
  LstFun f ls = app (mkN2 f) (mkNP and_Conj ls) ;

  --list stuff

--BaseNat : Nat -> Nat -> ListNat ;
--ConsNat : Nat -> ListNat -> ListNat ;
  BaseNumPred = mkListAP ;
  ConsNumPred = mkListAP ;

  BaseProp p q = {s = mkListS p.s q.s ; c = orC p.c q.c} ;
  ConsProp p ps = {s = mkListS p.s ps.s ; c = orC p.c ps.c} ;

  BaseNat = mkListNP ;
  ConsNat = mkListNP ;


}
