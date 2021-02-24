concrete ArithEng of Arith = ArithI with 
  (Syntax = SyntaxEng), 
  (Symbolic = SymbolicEng), 
  (Symbol = SymbolEng),
  (Sentence = SentenceEng)
  ** open (P = ParadigmsEng), ExtraEng, Prelude in {


lin

  Not p = {
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
  Odd x =
    { s = mkS (mkCl x.s (mkAP (P.mkA "odd"))) ; c = False } ;
  Even x =
    { s = mkS (mkCl x.s (mkAP (P.mkA "even"))) ; c = False } ;

  Equal x y =
    { s = mkS (mkCl x.s (P.mkA2 (P.mkA "equal") to_Prep) y.s) ; c = False } ;

  Add x y = {
    s = let f = mkFun2 "sum" in
      case <x.isSymbolic,y.isSymbolic> of {
        <True,True> => app2 f.s x.s y.s | app f.v x.s y.s ;
        _ => app f.v x.s y.s
      } ;
    isSymbolic = x.isSymbolic
    } ;

  Mul x y = {
    s = let f = mkFun2 "product" in
      case <x.isSymbolic,y.isSymbolic> of {
        <True,True> => app2 f.s x.s y.s | app f.v x.s y.s ;
        _ => app f.v x.s y.s
      } ;
    isSymbolic = x.isSymbolic
    } ;

  -- Fun2 = {s : Symb ; v : N2} ;
  -- IFun2  : Fun2 -> Ind -> Ind -> Ind ;

 --  IFun2 f x y = {
 --    s = case <x.isSymbolic,y.isSymbolic> of {
 --      <True,True> => app2 f.s x.s y.s | app f.v x.s y.s ;
 --      _ => app f.v x.s y.s
 --      } ;
 --    isSymbolic = x.isSymbolic
 --    } ;

  oper
    mkFun1, mkFun2 : Str -> {s : Symb ; v : N2} = \s -> 
      {s = mkSymb  ("\\" + s) ; v = P.mkN2 (P.mkN s)} ;


}

