concrete ArithSimple of Arith = open
  SyntaxEng,
  SymbolicEng,
  SymbolEng,
  SentenceEng,
  (P = ParadigmsEng), ExtraEng, Prelude in {

lincat

  Prop = S ;

  Ind, Var = NP ;

  -- Ind = NP ;
  -- Var = Symb ;

  Conj = SyntaxEng.Conj  ;

lin

  And = and_Conj ;
  Or = or_Conj ;

  -- PConj c p q = mkS c.s p.s q.s ;
  PConj = mkS ;

  If p q = mkS (mkAdv if_Subj p) (mkS then_Adv q) ;

  -- Not = negS ;

  Not p = mkS ExtraEng.UncNeg (mkCl
               (mkVP (mkNP the_Quant (mkCN case_N (mkAdv that_Subj p))))) ;


  Forall x p = mkS (mkAdv for_Prep (mkNP all_Predet x)) p ;

  -- Exist x p = mkS (existS (mkNP x (mkRS p))) ;

  -- Forall v p =
  --   ExtAdvS (mkAdv for_Prep (mkNP all_Predet (mkNP (SymbPN v)))) p ;


  -- "there is an element Foo such that Foo is a number"
  --        * [      *                                 ]

  -- Exist : Var -> Prop -> Prop
  Exist v p =
    mkS
     (mkCl
        (mkNP
           a_Quant
           (mkCN
              (mkCN
                 element_N
                 v)
              (mkAP
                 (mkAP such_A)
                 p)))) ;




  -- Exist v p = {
  --   s = mkS (mkCl (mkNP a_Quant (mkCN (mkCN element_N (mkNP (SymbPN v)))
  --                                  (mkAP (mkAP such_A) p.s)))) ;

  -- Github Code
  -- IVar x = mkNP (SymbPN x) ;
  -- VStr s = mkSymb s.s ;

  -- IVar          : Var  -> Ind ;
  -- VStr          : String -> Var ;
  --Paper for dummy lang
  IVar x = x ;
  VStr s = symb s ;

  -- (mkSymb s))

  IInt = symb ;
  Add = app (P.mkN2 (P.mkN "sum")) ;
  Mul = app (P.mkN2 (P.mkN "product")) ;

  Nat x =
    mkS (mkCl x (mkCN (P.mkN "number"))) ;
  Odd x =
    mkS (mkCl x (mkAP (P.mkA "odd"))) ;
  Even x =
    mkS (mkCl x (mkAP (P.mkA "even"))) ;

  -- Equal x y =
  --   mkS (mkCl
  --          x
  --          (P.mkA2 (P.mkA "equal"))
  --          y) ;

  Equal x y =
    mkS (mkCl x (P.mkA2 (P.mkA "equal") to_Prep) y) ;

  -- Equal = P.mkA2 (P.mkA "equal") to_Prep ;


  -- Nat = pred (mkCN (P.mkA "natural") (P.mkN "number")) ;
  -- Even = pred (P.mkA "even") ;
  -- Odd = pred (P.mkA "odd") ;
  -- Equal = pred (P.mkA "equal") ;

oper
  then_Adv = P.mkAdv "then" ;
  element_N = P.mkN "element" ;
  such_A = P.mkA "such" ;
  case_N = P.mkN "case" ;

}
