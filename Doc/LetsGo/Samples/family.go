family{
  import partner.
  import parent.
  import ancestor.
  import meta.
  import dynamicRel.
  import dynamicRules.
  import go.io.
  

  buildTree:[]*.
  buildTree() ->
      Parent.assert(('John I','John II'));
      Parent.assert(('John I','Jim'));
      Parent.assert(('John I','Jill'));
      Parent.assert(('Joan','John II'));
      Parent.assert(('Joan','Jim'));
      Parent.assert(('Joan','Jill'));
      Partner.assert(('John I','Joan'));

      Partner.assert(('Peter','Sue'));
      Parent.assert(('Peter','Mike'));
      Parent.assert(('Peter','Angus'));
      Parent.assert(('Sue','Mike'));
      Parent.assert(('Sue','Angus'));

      Partner.assert(('Mike','Jill'));
      Parent.assert(('Mike','Jill 2'));
      Parent.assert(('Mike','Marge'));
      Parent.assert(('Jill','Jill 2'));
      Parent.assert(('Jill','Marge'));
      
      Partner.assert(('Charlie','Jill 2'));
      Parent.assert(('Charlie','Jane'));
      Parent.assert(('Jill 2','Jane'));

      Partner.assert(('Bill','Marge'));
      Parent.assert(('Marge','John III'));
      Parent.assert(('Bill','John III')).

  main(_) ->
      buildTree();
--      (parent(X,Y).satisfy() *> stdout.outLine(X^<>" parent of "<>Y^));
--      (parent('John I',Y).satisfy() *> stdout.outLine("John I parent of "<>Y^));
--      (ancestor('John I',Y).satisfy() *> stdout.outLine("John I ancestor of "<>Y^));
      (ancestor(X,Y).satisfy() *> stdout.outLine(X^<>" ancestor of "<>Y^)).
  

}

  

