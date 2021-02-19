incomplete concrete ArithI of Arith = open
  Syntax,
  Symbolic,
  Symbol,
  Sentence, ----
  Prelude in {

lincat
  Prop = {s : S ; c : Bool} ; -- c = True for connectives
  Var = Symb ;
  Ind  = {s : NP ; isSymbolic : Bool} ;
  Conj = {s : Syntax.Conj ; c : S} ;  -- s = and ; c = all these hold

lin

  VStr s = mkSymb s.s ;
  IInt i = {s = symb i.s ; isSymbolic = True} ;
  If p q = {s = ExtAdvS (mkAdv if_Subj p.s) (mkS then_Adv q.s) ; c = True} ;
  PConj c p q = {s = mkS c.s p.s q.s ; c = True} ; -- can be ambiguous; cf. PConjs
  Forall v p = {
    s = ExtAdvS (mkAdv for_Prep (mkNP all_Predet (mkNP (SymbPN v)))) p.s ;
    c = False
    } ;
  Exist v p = { 
    s = mkS (mkCl (mkNP a_Quant (mkCN (mkCN element_N (mkNP (SymbPN v))) 
                                   (mkAP (mkAP such_A) p.s)))) ;
    c = False
    } ;

  CAnd = {s = and_Conj ; c = mkS (mkCl (mkNP all_Predet these_NP) hold_V)} ;
  COr = {
    s = or_Conj ; 
    c = mkS (mkCl (mkNP (mkNP (mkDet (mkCard at_least_AdN (mkCard "1")))) (mkAdv part_Prep these_NP)) hold_V)
    } ;

-- symbolic applications by LaTeX macros

oper
  app1 : Symb -> NP -> NP = \f,x -> symbNP (f.s ++ "{" ++ (mkUtt x).s ++ "}") ; 
  app2 : Symb -> NP -> NP -> NP = \f,x,y -> 
    symbNP (f.s ++ "{" ++ (mkUtt x).s ++ "}" ++ "{" ++ (mkUtt y).s ++ "}") ; 

  symbNP : Str -> NP = \s -> mkNP (SymbPN (mkSymb s)) ;
  -- symbNP : Str -> NP = \s -> mkNP (Symbol.SymbPN (mkSymb s)) ;


--- abuse of Conj category and its accidentally shared implementation

  bulletConj = lin Conj {s1,s2 = "\\item" ; n = singular} ;
  colonConj = lin Conj {s1 = [] ; s2 = ":" ; n = singular} ;


}
