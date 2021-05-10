concrete QueryEngRgl of Query = open
  SyntaxEng,
  SymbolicEng,
  ConstructorsEng,
  ParadigmsEng,
  ExtraEng, -- for the negative prop
  Prelude in {

lincat
  -- Answer = Polr => Text ;
  Answer = Text ;
  Object , Nat = NP ;
  Question = Utt ;
  [Nat] = [NP] ;
  Fun2 = N ;
  NumPred = AP ;
  [NumPred] = [AP] ;

  Prop = { s : S ; c : Compl } ;
  [Prop] = { s : [S] ; c : Compl } ; --TODO add extra parameter
  -- [Prop] = { s : Compl => [S] ; c : Compl } ; -- add commas or bullets depending on the compl

  Conj = { s : SyntaxEng.Conj ; c : S }  ;

lin

  -- logic --

--YesProp : Prop -> Answer ;
--NoProp : Prop -> Answer ;
  YesProp p = yesno yes_Utt p.s ;
  NoProp p = yesno no_Utt (negate p.s) ;


-- is it the case that _PROP_
--PropQuest : Prop -> Question ;
  PropQuest p =
    mkUtt (mkQS (theCaseThat p.s)) ;


--IsNumProp : NumPred -> Object -> Prop ;
  IsNumProp odd obj = { s = mkS (mkCl obj odd) ; c = Simp } ;

--LstNumProp : Conj -> [NumPred] -> NumPred ; -- E
  LstNumProp c = mkAP c.s ;

  -- mkS 	Conj -> S -> S -> S

  --LstProp : Conj  -> [Prop] -> Prop ; -- E
  LstProp c ps = case ps.c of {
    Com  => {s = mkS colonConj c.c (mkS bulletConj ps.s) ; c = Simp } ;
    Simp => {s = mkS c.s ps.s ; c = Com }
    } ;

--If : Prop -> Prop -> Prop ;
  If p q = { s = mkS (ConstructorsEng.mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = Simp } ;
  -- PImpl p q = {s = ExtAdvS (mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = True} ;

  -- is it the case that it is not the case that 999 is even
  -- can now be normalized

--Not : Prop -> Prop ;
  Not p =
    {
      s = mkS
        ExtraEng.UncNeg
        (theCaseThat p.s) ;
      c = Simp
    } ;

--PConj : Conj -> Prop -> Prop -> Prop ;
  PConj c p1 p2 = { s = mkS c.s p1.s p2.s ; c = Com } ;

--And, or : Conj ;
  -- And = and_Conj ;
  -- Or = or_Conj ;

  And = {s = and_Conj ; c = mkS (mkCl (mkNP all_Predet these_NP) hold_V)} ;
  Or = {
    s = or_Conj ;
    c = mkS (mkCl (mkNP (mkNP (mkDet (mkCard at_least_AdN (mkCard "1")))) (ConstructorsEng.mkAdv part_Prep these_NP)) hold_V)
    } ;

-- Question Answer --

--YesIsNumPred : NumPred -> Object -> Answer ;
--NoIsNumPred : NumPred -> Object -> Answer ;
  -- YesIsNumPred = yesno Pos yes_Utt ;
  -- NoIsNumPred = yesno Neg no_Utt ;

  Yes = yesno yes_Utt ;
  No = yesno no_Utt ;

------ Natural Number Domain -------

--IsNumPred : NumPred -> Object -> Question ;
  -- IsNumPred = isNumericPred ;

--Even , Odd, Prime : NumPred ;
  Even = numprop "even" ;
  Odd = numprop "odd" ;
  Prime = numprop "prime" ;

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
  BaseNumPred = mkListAP ;
  ConsNumPred = mkListAP ;

  BaseProp p q = {s = mkListS p.s q.s ; c = orC p.c q.c} ;
  ConsProp p ps = {s = mkListS p.s ps.s ; c = orC p.c ps.c} ;

  BaseNat = mkListNP ;
  ConsNat = mkListNP ;

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

---- TODO

  -- all for this failed thing, ask inari
  -- YesNo = table  {Pos => yesno yes_Utt ; Neg => yesno no_Utt} ;
  -- Yesnoisnumpred x y = Table {Pos => yesno Pos yes_Utt x y ; Neg => yesno Neg no_Utt x y } ;
  --Yesisnumpred : numpred -> Object -> Answer ;
  --NoIsNumPred : NumPred -> Object -> Answer ;
  -- YesIsNumPred = yesno Pos yes_Utt ;
  -- NoIsNumPred = yesno Neg no_Utt ;


  -- keep to remind that we need to break stuff apart
  -- isNumericPred : AP -> NP -> Utt ;
  -- isNumericPred even obj = mkUtt (mkQS (mkCl obj even)) ;
  -- Prop = { prop = AP ; ob = NP } ; -- but then need to refactor, but need to do anyways

}
