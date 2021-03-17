abstract Query = {

flags startcat=Question ;

cat
  Answer ; Question ; Object ; Nat ;
  [Nat]{2};
  Fun2 ;
  NumPred ;

  Conj;
  Prop;

fun

  -- (boolean valued) prop logic
  -- so how to abstract the question and answer layers
  --when to introduce variables?

  YesProp : Prop -> Answer ;
  NoProp : Prop -> Answer ;

  PropQuest : Prop -> Question ;

  IsNumProp : NumPred -> Object -> Prop ;

  If            : Prop -> Prop -> Prop ;
  Not           : Prop -> Prop ;
  PConj : Conj -> Prop -> Prop -> Prop ;

  And, Or : Conj ;

  --- added ---

  Yes : Answer ;
  No : Answer ;

  -- YesIsNumPred : NumPred -> Object -> Answer ;
  -- NoIsNumPred  : NumPred -> Object -> Answer ;

  -- IsNumPred : NumPred -> Object -> Question ;

  NatObj : Nat -> Object ;

  -- all arith below
  Even , Odd, Prime : NumPred ; --Pred

  BinFun : Fun2 -> Nat -> Nat -> Nat ;
  LstFun  : Fun2 -> [Nat] -> Nat ;

  Plus : Fun2 ;
  Times : Fun2 ;

  Number : Int -> Nat ;

  -- BaseNat : Nat -> Nat -> ListNat ;
  -- ConsNat : Nat -> ListNat -> ListNat ;

  -----------------------------------------
  -- but why not just have it unary so it just appends an and at the end -- i think it has to do with commas

  --TODO
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
