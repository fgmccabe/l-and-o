parent{
  import meta.
  import dynamicRel.

  Parent:dynRel[(symbol,symbol)] = dynamicRel([]).

  parent:[symbol,symbol]$=metaTp.
  parent(A,B)..{
    satisfy() :-
        Parent.mem((A,B)).
  }.
}
