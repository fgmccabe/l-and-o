dynamicRel{
  import go.dynamic.

  dynRel[T] <~ { assert:[T]*. }.
  dynRel[T] <~ dynamic[T].

  dynamicRel:[list[T]]@>dynRel[T].
  dynamicRel(I) <= dynamic(I).
  dynamicRel(_)..{
    mem(X) :-
        dynamic.mem(X).
    assert(X) ->
        add(X).
  }.
}