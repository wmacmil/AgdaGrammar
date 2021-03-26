abstract Query = {

flags startcat=Question ;

cat
  Answer ; Question ; Object ; Nat ;
  [Nat]{2};
  Fun2 ;
  NumPred ;
  [NumPred]{2};

  Conj;
  Prop;
  [Prop]{2};

fun

  --E means extended

  --so at the syntax level of semantics (PGF tree abstraction) one is essentially taking for granted and using theorems of propositional logic in order to generate trees.

  YesProp : Prop -> Answer ;
  NoProp : Prop -> Answer ;

  PropQuest : Prop -> Question ;

  IsNumProp : NumPred -> Object -> Prop ;
  LstNumProp : Conj -> [NumPred] -> NumPred ; -- E
  LstProp : Conj  -> [Prop] -> Prop ; -- E

  If            : Prop -> Prop -> Prop ;
  Not           : Prop -> Prop ;
  PConj : Conj -> Prop -> Prop -> Prop ;

  And, Or : Conj ;

  --- added ---

  -- ShortAns : Answer ;

  Yes : Answer ;
  No : Answer ;

  NatObj : Nat -> Object ;

  -- all arith below
  Even , Odd, Prime : NumPred ; --Pred

  BinFun : Fun2 -> Nat -> Nat -> Nat ;
  LstFun  : Fun2 -> [Nat] -> Nat ; -- E

  Plus : Fun2 ;
  Times : Fun2 ;

  Number : Int -> Nat ;

  -- BaseNat : Nat -> Nat -> ListNat ;
  -- ConsNat : Nat -> ListNat -> ListNat ;

  -----------------------------------------
  -- but why not just have it unary so it just appends an and at the end -- i think it has to do with commas

  --TODO
  -- can also do a curry
  -- if a then if b then c --> if a and b then c
  -- if n is even then if n is prime then n = 2
  -- if n is even and if n if prime then n = 2
  -- if n is even and prime then n = 2
  -- if n is an even prime then n = 2
  -- 2 is the only prime
  
  -- note this is somewhat of a musing daydream as well as concrete things that should be done
  -- add more types, say bool and String
  -- Aarne distinguishes Kinds and Preds in Cade, but in Agda these aren't concrete distinctions
  --add logic layer
  -- Mersenne Prime, Sophie Germain prime, etc.
  -- Then can include things like amicable number (pairs of numbers), coprime numbers, etc.
  -- primality isn't defined inductively, but 
  -- https://github.com/agda/agda-stdlib/blob/master/src/Data/Nat/Primality.agda
  -- proof by reflection comes in

  -- could further refine this by including
  -- Cat NatProp ;
  -- Even , Odd , Prime : NatProp ;
  -- IsProp : NatProp -> Object -> Question ;
  -- could even further refine using depedent types
  -- where one then introduces Kinds (Nat, Int, Vector Space) and
  -- and then have Props, Objections and questions depedent on Kind

}
