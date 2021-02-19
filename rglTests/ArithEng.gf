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

  Odd x =
    { s = mkS (mkCl x.s (mkAP (P.mkA "odd"))) ; c = False } ;

  Even x =
    { s = mkS (mkCl x.s (mkAP (P.mkA "even"))) ; c = False } ;

  -- Square = mkFun1 "square" ;
  -- Sum = mkFun2 "sum" ;
  -- Product = mkFun2 "product" ;

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


    -- p -cat=Ind "the sum of x and y"
    --   IFun2 Sum (IVar (VString "x")) (IVar (VString "y"))
    --   -- IFunC Sum (BaseInd (IVar (VString "x")) (IVar (VString "y")))

  -- IFun1 f x = {
  --   s = case x.isSymbolic of {
  --     True  => app1 f.s x.s | app f.v x.s ; -- preferred symbolic, allowed verbal
  --     False => app f.v x.s
  --     } ;
  --   isSymbolic = x.isSymbolic
  --   } ;



  -- APred1 f x = mkCl x.s f ;
  -- APred1 : Pred1 -> Ind -> Atom ;

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
