abstract Query = Arith ** {
  -- where Arith =  PropExt **...

flags startcat=Question ;

cat
  Answer ; Question ;

fun

  YesProp : Prop -> Answer ;
  NoProp : Prop -> Answer ;

  PropQuest : Prop -> Question ;

  Yes : Answer ;
  No : Answer ;

}
