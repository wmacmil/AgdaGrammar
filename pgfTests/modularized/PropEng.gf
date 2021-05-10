concrete PropEng of Prop = ArithEng ** open
  Helpers,
  -- rgl
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  ExtraEng, -- for the negative prop
  Prelude in {

lincat


  Prop = { s : S ; c : Compl } ;
  Conj = { s : SyntaxEng.Conj ; c : S }  ;

  -- [Prop] = { s : [S] ; c : Compl } ; --TODO add extra parameter
  -- [Nat] = [NP] ;
  -- [NumPred] = [AP] ;
  -- -- [Prop] = { s : Compl => [S] ; c : Compl } ; -- add commas or bullets depending on the compl


lin

  --PConj : Conj -> Prop -> Prop -> Prop ;
  PConj c p1 p2 = { s = mkS c.s p1.s p2.s ; c = Com } ;

  --IsNumProp : NumPred -> Object -> Prop ;
  IsNumProp odd obj = { s = mkS (mkCl obj odd) ; c = Simp } ;

  --If : Prop -> Prop -> Prop ;
  If p q = { s = mkS (ConstructorsEng.mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = Simp } ;
  -- PImpl p q = {s = ExtAdvS (mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = True} ;

  --Not : Prop -> Prop ;
  Not p =
    {
      s = mkS
        ExtraEng.UncNeg
        (theCaseThat p.s) ;
      c = Simp
    } ;


  And = {s = and_Conj ; c = mkS (mkCl (mkNP all_Predet these_NP) hold_V)} ;
  Or = {
    s = or_Conj ;
    c = mkS (mkCl (mkNP (mkNP (mkDet (mkCard at_least_AdN (mkCard "1")))) (ConstructorsEng.mkAdv part_Prep these_NP)) hold_V)
    } ;


}
