abstract Exp = {

flags startcat = Decl ;


      -- note, cubical tt doesn't support inductive families, and therefore the datatype (& labels) need to be modified

cat
  Comment ;
  Module  ;
  AIdent ;
  Imp ; --imports, add later
  Decl ; 
  Exp ;
  ExpWhere ;
  Tele ;
  Branch ; 
  PTele ;
  Label ;
  [AIdent]{0} ; -- "x y z"
  [Decl]{1} ; 
  [Tele]{0} ;
  [Branch]{1} ; 
  -- [Branch]{0} ; 
  [Label]{1} ;
  [PTele]{1} ; 
  -- [Exp]{1};

  --cat [C] {n}
  -- =
  --cat ListC ;
  --fun BaseC : C -> ...-> C -> ListC ; -- n C ’s
  --fun ConsC : C -> ListC -> ListC

fun

  DeclDef : AIdent -> [Tele] -> Exp -> ExpWhere -> Decl ;
  -- foo ( b : bool ) : bool = b
  DeclData : AIdent -> [Tele] -> [Label] -> Decl ; 
  -- data nat : Set where zero | suc ( n : nat )
  DeclSplit : AIdent -> [Tele] -> Exp -> [Branch] -> Decl ;
  -- caseBool ( x : Set ) ( y z : x ) : bool -> Set = split false -> y || true -> z
  DeclUndef : AIdent -> [Tele] -> Exp -> Decl ;
  -- funExt ( a : Set ) ( b : a -> Set ) ( f g : ( x : a ) -> b x ) ( p : ( x : a ) -> ( b x ) ( f x ) == ( g x ) ) : ( ( y : a ) -> b y ) f == g = undefined


  Where : Exp -> [Decl] -> ExpWhere ;
  -- foo ( b : bool ) : bool =
  -- f b where f : bool -> bool = negb
  NoWhere : Exp -> ExpWhere ;
  -- foo ( b : bool ) : bool =
  -- b

  Split : Exp -> [Branch] -> Exp ;
  --split@ ( nat -> bool ) with zero  -> true || suc n -> false 
  Let : [Decl] -> Exp -> Exp ;
  -- foo ( b : bool ) : bool =
  -- let f : bool -> bool = negb in f b
  Lam : [PTele] -> Exp -> Exp ;
  -- \\ ( x : bool ) -> negb x
  -- todo, allow implicit typing
  Fun : Exp -> Exp -> Exp ;
  -- Set -> Set
  -- Set -> ( b : bool ) -> ( x : Set ) -> ( f x )
  Pi    : [PTele] -> Exp -> Exp ;
  --( f : bool -> Set ) -> ( b : bool ) -> ( x : Set ) -> ( f x )
  -- ( f : bool -> Set ) ( b : bool ) ( x : Set ) -> ( f x )
  Sigma : [PTele] -> Exp -> Exp ;
  -- ( f : bool -> Set ) ( b : bool ) ( x : Set ) * ( f x )
  App : Exp -> Exp -> Exp ;
  -- proj1 ( x , y )
  Id : Exp -> Exp -> Exp -> Exp ;
  -- Set bool == nat
  IdJ : Exp -> Exp -> Exp -> Exp -> Exp -> Exp ;
  -- J e d x y p
  Fst : Exp -> Exp ; -- "proj1 x"
  Snd : Exp -> Exp ; -- "proj2 x"
  -- Pair : Exp -> [Exp] -> Exp ;
  Pair : Exp -> Exp -> Exp ;
  -- ( x , y )
  Var : AIdent -> Exp ;
  -- x
  Univ : Exp ;
  -- Set
  Refl : Exp ;
  -- refl
  --Hole : HoleIdent -> Exp ; -- need to add holes

  OBranch :  AIdent -> [AIdent] -> ExpWhere -> Branch ;
  -- suc m -> split@ ( nat -> bool ) with zero -> false || suc n -> equalNat m n
  -- for splits

  OLabel : AIdent -> [Tele] -> Label ;
  -- suc ( n : nat ) 
  -- fora data types

  -- construct telescope
  TeleC : AIdent -> [AIdent] -> Exp -> Tele ; 
  -- "( f g : ( x : a ) -> b x )"
  -- ( a : Set ) ( b : ( a ) -> ( Set ) ) ( f g : ( x : a ) -> ( ( b ) ( x ) ) ) ( p : ( x : a ) -> ( ( ( b ) ( x ) ) ( ( f ) ( x ) ) == ( ( g ) ( x ) ) ) )

  -- why does gr with this fail so epically?
  PTeleC : Exp -> Exp -> PTele ; -- ( x : Set ) -- ( y : x -> Set )" -- ( x : f y z )"

  --everything below this is just strings

  Foo : AIdent ;

  A , B , C , D , E , F , G , H , I , J , K , L , M , N , O , P , Q , R , S , T , U , V , W , X , Y , Z : AIdent ;

  True , False , Bool : AIdent ;
  NegB : AIdent ;
  CaseBool : AIdent ;
  IndBool : AIdent ;

  FunExt : AIdent ;

  Nat : AIdent ;
  Zero : AIdent ;
  Suc : AIdent ;
  EqualNat : AIdent ;

  Unit : AIdent ;
  Top : AIdent ;

}
