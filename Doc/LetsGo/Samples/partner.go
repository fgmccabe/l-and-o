partner{
  import meta.
  import dynamicRel.

  Partner:dynRel[(symbol,symbol)] = dynamicRel([]).

  partner:[symbol,symbol]$=metaTp.
  partner(A,B)..{
    satisfy() :-
        Partner.mem((A,B)).
  }.
}
