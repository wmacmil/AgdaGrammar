concrete ArithEng of Arith = ArithI with 
  (Syntax = SyntaxEng), 
  (Symbolic = SymbolicEng), 
  (Symbol = SymbolEng),
  (Sentence = SentenceEng)
  ** open (P = ParadigmsEng), ExtraEng, Prelude in {


lin

  PNeg p = {
    s = mkS ExtraEng.UncNeg (mkCl
                (mkVP (mkNP the_Quant (mkCN case_N (mkAdv that_Subj p.s))))) ;
    c = False ---- ?
    } ;

oper
  hold_V = P.mkV "hold" "held" "held" ;
  such_A = P.mkA "such" ;
  element_N = P.mkN "element" ;
  case_N = P.mkN "case" ;
  then_Adv = P.mkAdv "then" ;

  singular = P.singular ; ---

lin

  Nat x =
    { s = mkS (mkCl x.s (mkCN (P.mkN "number"))) ; c = False } ;
  -- PAtom a = {s = mkS a ; c = False} ;
 -- AKind k x = mkCl x.s k ;
  -- Nat = mkCN (P.mkN "number") ;

}

  -- And p1 p2 = { s = "" } ;
  -- And, Or, If   : Prop -> Prop -> Prop ;
  -- Not           : Prop -> Prop ;
  -- Forall, Exist : Var  -> Prop -> Prop ;
  -- IVar          : Var  -> Ind ;
  -- IVar x = {s = mkNP (SymbPN x) ; isSymbolic = True} ;
  -- IVar x = {s = mkNP (SymbPN x) }; -- isSymbolic = True} ; --

  -- VStr s = mkSymb s.s ;

  -- Nat = mkCN (P.mkN "number") ;

  -- IInt           : Int -> Ind ;
  -- Add, Mul       : Ind -> Ind -> Ind ;
  -- Nat, Even, Odd : Ind -> Prop ;
  -- Equal          : Ind -> Ind -> Prop ;
